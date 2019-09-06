{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Data.Char (chr)
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Typeable (cast)
import Data.Word (Word8)
import Foreign (Ptr, allocaArray, peekArray)
import System.Exit (exitSuccess)
import System.IO (Handle, IOMode(ReadMode), hGetBuf, hPutStrLn, withBinaryFile)
import System.IO.Error (eofErrorType, mkIOError)
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Hooks.DynamicLog hiding (statusBar)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks
  ( AvoidStruts
  , ToggleStruts(ToggleStruts)
  , avoidStruts
  , docks
  , docksEventHook
  )
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.StackSet (focusDown)
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main =
  xmonad <=< myXmobar $
  ewmh (myConf def) `additionalKeysP` myKeys `removeKeysP` myDisabledKeys

myConf conf =
  conf
    { modMask = mod4Mask -- use Super instead of Alt
    , terminal = "term"
    , borderWidth = 3
    , normalBorderColor = "#000"
    , focusedBorderColor = "#63ff3b"
    , handleEventHook =
        handleEventHook conf <> docksEventHook <> fullscreenEventHook
    , layoutHook = myLayoutHook
    , manageHook = myManageHook <+> manageHook conf
    }

myXmobar = statusBar "xmobar" myXmobarPP myToggleStrutsKey

myXmobarPP =
  def
    { ppTitle = color "#fff" ""
    , ppCurrent = color "#fff" ""
    , ppVisible = color "#65cf24" ""
    , ppHidden = color "#559e24" ""
    , ppHiddenNoWindows = color "#555" ""
    , ppUrgent = color "#ff3311" ""
    , ppLayout = color "#65cf24" ""
    , ppSep = " | "
    , ppWsSep = " "
    }
  where
    color = xmobarColor

myLayoutHook = tiled ||| reflectHoriz tiled ||| Mirror tiled ||| Full
  where
    tiled =
      ResizableTall
        1 {- default number of windows in master pane -}
        0.02 {- resize increment    [fraction of screen] -}
        0.5 {- size of master pane [fraction of screen] -}
        []

myManageHook =
  composeAll
    [ className =? "Xfce4-notifyd" --> doIgnore
    , List.isInfixOf "Figure" <$> title --> doFloat
    ]

myKeys =
  [ ("M-S-<Delete>", io exitSuccess)
  , ("M-v", windows copyToAll) -- @@ Make focused window always visible
  , ("M-S-v", killAllOtherCopies) -- @@ Toggle window state back
  ]

myDisabledKeys = ["M-S-q"]

myToggleStrutsKey conf = (modMask conf, xK_b)

-- | Prevent new windows from stealing focus.
avoidFocusStealing = doF focusDown

statusBar ::
     LayoutClass l Window
  => String
  -> PP
  -> (XConfig Layout -> (KeyMask, KeySym))
  -> XConfig l
  -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar cmd pp k conf = do
  key <- newStateExtensionKey
  pure . docks $
    conf
      { layoutHook = avoidStruts (layoutHook conf)
      , logHook =
          do logHook conf
             m <- getStateExtension key
             h <-
               case m of
                 Just h -> pure h
                 _ -> do
                   liftIO
                     (throwIO (userError "statusBar: can't retrieve handle"))
             dynamicLogWithPP pp {ppOutput = hPutStrLn h}
      , keys = Map.union <$> keys' <*> keys conf
      , startupHook =
          do h <- spawnPipe cmd
             putStateExtension key h
      }
  where
    keys' = (`Map.singleton` sendMessage ToggleStruts) . k

newtype StateExtensionKey a =
  StateExtensionKey String

newStateExtensionKey :: IO (StateExtensionKey a)
newStateExtensionKey = do
  let n = 32
  let path = "/dev/urandom"
  withBinaryFile path ReadMode $ \h -> do
    allocaArray n $ \p -> do
      m <- hGetBuf h (p :: Ptr Word8) n
      if m == n
        then StateExtensionKey . (chr . fromIntegral <$>) <$> peekArray n p
        else throwIO (mkIOError eofErrorType "end of file" (Just h) (Just path))

newtype StateExtensionValue a =
  StateExtensionValue (Maybe a)
  deriving (Typeable)

instance Typeable a => ExtensionClass (StateExtensionValue a) where
  initialValue = StateExtensionValue Nothing

getStateExtension ::
     (MonadState XState m, Typeable a) => StateExtensionKey a -> m (Maybe a)
getStateExtension (StateExtensionKey key) = do
  get <&> \s -> do
    case Map.lookup key (extensibleState s) of
      Just (Right (StateExtension x)) ->
        case cast x of
          Just (StateExtensionValue (Just value)) -> Just value
          _ -> Nothing
      _ -> Nothing

putStateExtension ::
     (MonadState XState m, Typeable a) => StateExtensionKey a -> a -> m ()
putStateExtension (StateExtensionKey key) value = do
  modify $ \s -> do
    s
      { extensibleState =
          Map.insert
            key
            (Right (StateExtension (StateExtensionValue (Just value))))
            (extensibleState s)
      }
