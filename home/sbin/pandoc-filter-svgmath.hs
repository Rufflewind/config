#!/usr/bin/env runhaskell
-- | Pandoc filter for converting TeX math into SVGs.
--
-- Usage:
--
-- > ghc -hide-package=cryptohash \
-- >     -o "$HOME/.local/bin/pandoc-filter-svgmath" \
-- >     "$HOME/.local/sbin/pandoc-filter-svgmath.hs"
-- > pandoc --filter pandoc-filter-svgmath …
--
-- Dependencies:
--
-- * pdflatex
-- * pdf2svg
--
-- Known limitations:
--
-- * Only inline math constructs like @aligned@ work, but this is
--   usually not a problem since there is almost always an equivalent.
--
-- Based on https://gist.github.com/rwst/1437841 which was
-- originally written by Ralf Stephan and John MacFarlane.
--
module Main (main) where
import Control.Applicative
import Control.Arrow (first)
import Control.Exception (bracket, onException, throwIO)
import Control.Monad
import Crypto.Hash (Digest, MD5)
import Data.Foldable (fold)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Monoid
import Data.Set (Set)
import System.Directory (createDirectoryIfMissing, doesFileExist,
                         getCurrentDirectory, getTemporaryDirectory,
                         renameFile, setCurrentDirectory)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Error (catchIOError, doesNotExistErrorType,
                        isDoesNotExistError, mkIOError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Text.Pandoc.JSON
import Text.Regex.Posix ((=~))
import Prelude
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Lazy as BytesL
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.XML.Light as XML

type TheDigest = Digest MD5

main :: IO ()
main = do
  progContents <- BytesL.readFile =<< getExecutablePath
  let progHash = show (Hash.hashlazy progContents :: TheDigest)
  seenSvgsRef <- newIORef mempty
  toJSONFilter (mathToSvgFilter progHash seenSvgsRef)

mathToSvgFilter :: String -> IORef (Set String) -> Inline -> IO Inline
mathToSvgFilter progHash seenSvgsRef (Math displayStyle code) =
  (`onException` onError) $ do
    svg <- mathToSvg progHash seenSvgsRef style (polishTex (codePrefix <> code))
    pure (RawInline (Format "html") svg)
  where

    style = case displayStyle of
      DisplayMath -> "display:block"
      InlineMath  -> ""

    onError =
      reportError ("pandoc-svg: failed to convert equation: " <> code)

    codePrefix =
      case displayStyle of
        DisplayMath -> "\\displaystyle "
        InlineMath  -> ""

mathToSvgFilter _ _ x = pure x

polishTex :: String -> String
polishTex = removeNewline

mathClass :: String
mathClass = "math"

polishSvg :: Maybe XML.Element -> String -> String -> Maybe String
polishSvg repl style svg =
  XML.showElement .
  replaceContent .
  XML.add_attr (XML.Attr (XML.unqual "class") mathClass) .
  XML.add_attr (XML.Attr (XML.unqual "style")
                         ("vertical-align:middle;" <> style)) <$>
  (scaleSize =<< XML.parseXMLDoc svg)
  where

    scaleSize =
      traverseXmlAttr (XML.unqual "height")
      ((showCssLength . first (* scale) <$>) . parseCssLength) <=<
      traverseXmlAttr (XML.unqual "width")
      ((showCssLength . first (* scale) <$>) . parseCssLength)

    replaceContent root =
      case repl of
        Nothing -> root
        Just r  -> root{XML.elContent = [XML.Elem r]}

scale :: Float
scale = 1.3

-- | Parse @"<number><unit>"@ into @(number, unit)@.
parseCssLength :: String -> Maybe (Float, String)
parseCssLength s =
  case s =~ "([-+0-9.]+)([a-z%]*)" of
    [[_, n, u]] -> readMaybe n >>= \ x -> Just (x, u)
    _           -> Nothing

-- | Reverse of 'parseCssLength'.
showCssLength :: (Float, String) -> String
showCssLength (x, u) = show x <> u

traverseXmlAttr :: Applicative f =>
                   XML.QName
                -> (String -> f String)
                -> XML.Element
                -> f XML.Element
traverseXmlAttr name f el =
  (\ x -> el{XML.elAttribs = x}) <$> go (XML.elAttribs el)
  where go []         = pure []
        go (a@(XML.Attr n v) : xs)
          | n == name = (: xs) . XML.Attr n <$> f v
          | otherwise = (a :) <$> go xs

readMaybe :: Read a => String -> Maybe a
readMaybe s =
  case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

hashString :: Hash.HashAlgorithm a => String -> Digest a
hashString = Hash.hash . Text.encodeUtf8 . Text.pack

mathToSvg :: String -> IORef (Set String) -> String -> String -> IO String
mathToSvg progHash seenSvgsRef style equation = do

  tempDir <- getTemporaryDirectory
  let cacheDir  = tempDir </> "pandoc-svg-cache"
      mathHash  = show (hashString (show equation) :: TheDigest)
      svgId     = "math-" <> mathHash
      cacheFilename = cacheDir </> progHash <> "-" <> mathHash <> ".svg"
  createDirectoryIfMissing True cacheDir

  fileExists <- doesFileExist cacheFilename

  unless (enableCache && fileExists) $
    withSystemTempDirectory "pandoc-svg" $ \ dir ->
      withCurrentDirectory dir $ do
        writeFile "texput.tex" texCode
        silentCall "pdflatex" ["-interaction=nonstopmode", "texput.tex"]
        silentCall "pdf2svg" ["texput.pdf", "texput%d.svg", "all"]
        silentCall "sed" ["-i",
                          "s/ id=\"/ id=\"" <> svgId <>
                          "-/;s/xlink:href=\"#/xlink:href=\"#" <> svgId <>
                          "-/",
                          "texput1.svg"]
        renameFile "texput1.svg" cacheFilename
        pure ()

  svg <- readFile cacheFilename

  seenSvgs <- readIORef seenSvgsRef
  replacement <-
    if Set.member svgId seenSvgs
    then
      pure
      (Just
       (XML.Element
        (XML.unqual "use")
        [XML.Attr
         (XML.QName "href" Nothing (Just "xlink"))
         ("#" <> svgId <> "-surface1")]
        []
        Nothing))
    else do
      writeIORef seenSvgsRef (Set.insert svgId seenSvgs)
      pure Nothing

  case polishSvg replacement style svg of
    Nothing -> ioError (userError "invalid SVG")
    Just x  -> pure x

  where

    enableCache = True

    texCode =
      fold
      [ "\\documentclass{minimal}"
      , "\\usepackage[active,pdftex,tightpage]{preview}"
      , preamble
      , "\\begin{document}"
        -- note that '$' can be used to break out of this section; since TeX
        -- is just too powerful to tame, there's no way to plug this hole
        -- so you'll want to make sure the code is trusted!
      , "\\begin{preview}\\("
        -- make the vertical alignment more consistent
      , "\\vphantom{y^j_j}"
      , equation
      , "\\)\\end{preview}"
      , "\\end{document}"
      ]

removeNewline :: String -> String
removeNewline = filter (`notElem` "\r\n")

-- This has already existed in System.Directory since directory-1.2.3.0.
-- We implement our own for backward-compatibilty reasons.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

preamble :: String
preamble =
  fold
  [ "\\usepackage{amsmath}"
  , "\\usepackage{amssymb}"
  ]

silentCall :: FilePath -> [String] -> IO ()
silentCall p a = do
  (exit, out, err) <-
    readProcessWithExitCode p a ""
    `catchIOError` \ e ->
      if isDoesNotExistError e
      then throwIO (mkIOError doesNotExistErrorType "" Nothing (Just p))
      else throwIO e
  case exit of
    ExitSuccess   -> pure ()
    ExitFailure e -> ioError . userError $
                     p <> " exited with " <>
                     show e <> ":\n" <> out <> err

reportError :: String -> IO ()
reportError err = do
  hPutStrLn stderr err
  hFlush stderr
