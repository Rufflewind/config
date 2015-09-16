#!/usr/bin/env runhaskell
-- | Pandoc filter for converting TeX math into SVGs
--
-- Usage:
--
-- > ghc -o "$HOME/.local/bin/pandoc-svg" "$HOME/.local/sbin/pandoc-svg.hs"
-- > pandoc --filter pandoc-svg [-f SRCFMT] [-t DESTFMT] [-o OUTFILE] INFILE
--
-- Dependencies:
--
-- * pdflatex (obviously)
-- * pdf2svg (a very efficient pdf-to-svg converter)
--
-- Based on https://gist.github.com/rwst/1437841
-- which was written by Ralf Stephan and John MacFarlane.
--
module Main (main) where
import Control.Applicative
import Control.Exception (bracket, onException)
import Control.Monad
import Data.Byteable (toBytes)
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import System.Directory hiding (withCurrentDirectory)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Temp
import System.Process
import Text.Pandoc.JSON
import Prelude
import Crypto.Hash (Digest, MD5)
import qualified Data.ByteString.Lazy as BytesL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Crypto.Hash as Hash

type TheDigest = Digest MD5

main :: IO ()
main = do
  progContents <- BytesL.readFile =<< getExecutablePath
  toJSONFilter (mathToSvg (toBytes (Hash.hashlazy progContents :: TheDigest)))

showHash :: Digest a -> String
showHash = Text.unpack . Text.decodeUtf8 . Hash.digestToHexByteString

mathToSvg :: ByteString -> Inline -> IO Inline
mathToSvg progHash (Math displayStyle equation) = reportErrors $ do

  tempDir <- getTemporaryDirectory
  let cacheDir = tempDir </> "pandoc-svg-cache"
      input    = Text.encodeUtf8 (Text.pack (show (displayStyle, equation)))
      mathHash = showHash (Hash.hash (progHash <> input) :: TheDigest)
      cacheFilename = cacheDir </> mathHash <> ".svg"
  createDirectoryIfMissing True cacheDir

  fileExists <- doesFileExist cacheFilename

  hPutStrLn stderr texCode

  unless (enableCache && fileExists) $
    withSystemTempDirectory "pandoc-svg" $ \ dir ->
      withCurrentDirectory dir $ do
        writeFile "texput.tex" texCode
        silentCall "pdflatex" ["-interaction=nonstopmode", "texput.tex"]
        silentCall "pdf2svg" ["texput.pdf", "texput%d.svg", "all"]
        silentCall "sed" ["-i",
                          "s/ id=\"/ id=\"_" <> mathHash <>
                          "/;s/xlink:href=\"#/xlink:href=\"#_" <> mathHash <>
                          "/",
                          "texput1.svg"]
        renameFile "texput1.svg" cacheFilename
        pure ()

  svg <- readFile cacheFilename
  pure . RawInline (Format "html") $ case displayStyle of
    DisplayMath -> "<p>" <> svg <> "</p>"
    InlineMath  -> svg

  where

    enableCache = True

    reportErrors action = onException action $ do
      hPutStrLn stderr "pandoc-svg: failed to convert:"
      hPutStrLn stderr equation
      hFlush stderr

    texCode =
      fold
      [ "\\documentclass{minimal}"
      , "\\usepackage[active,pdftex,tightpage]{preview}"
      , preamble
      , "\\begin{document}"
        -- note that '$' can be used to break out of this section; since TeX
        -- code is just too powerful to tame, there's no way we can plug this
        -- hole entirely so you'll want to make sure the code is trusted!
      , "\\begin{preview}\\("
      , case displayStyle of
          DisplayMath -> "\\displaystyle "
          InlineMath  -> ""
      , removeNewline equation
      , "\\)\\end{preview}"
      , "\\end{document}"
      ]

mathToSvg _ x = pure x

removeNewline :: String -> String
removeNewline = filter (`notElem` "\r\n")

-- This has already existed in System.Directory since directory-1.2.3.0.
-- We implement our own for backward-compatibilty reasons.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

preamble =
  fold
  [ "\\usepackage{amsmath}"
  , "\\usepackage{amssymb}"
  ]

silentCall :: FilePath -> [String] -> IO ()
silentCall p a = do
  (exit, out, err) <- readProcessWithExitCode p a ""
  case exit of
    ExitSuccess   -> pure ()
    ExitFailure e -> do
      hPutStrLn stderr out
      hPutStrLn stderr err
      hFlush stderr
      ioError (userError (show (p : a) <> " exited with " <> show e <> ")"))
