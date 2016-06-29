{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
import Control.Monad ((<=<))
import Data.Monoid ((<>))
import System.Exit (exitSuccess)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (docksEventHook)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.StackSet (focusDown)
import XMonad.Util.EZConfig

main :: IO ()
main =
  xmonad <=< myXmobar $
    ewmh (myConf defaultConfig)
    `additionalKeysP` myKeys
    `removeKeysP` myDisabledKeys

myConf conf =
  conf
  { modMask = mod4Mask -- use Super instead of Alt
  , terminal = "term"
  , borderWidth = 3
  , normalBorderColor = "#000"
  , focusedBorderColor = "#63ff3b"
  , handleEventHook = handleEventHook conf
                   <> docksEventHook
                   <> fullscreenEventHook
  , layoutHook = myLayoutHook
  , manageHook = myManageHook <+> manageHook conf
  }

myXmobar = statusBar "xmobar" myXmobarPP myToggleStrutsKey

myXmobarPP =
  defaultPP
  { ppTitle           = color "#fff" ""
  , ppCurrent         = color "#fff" ""
  , ppVisible         = color "#65cf24" ""
  , ppHidden          = color "#559e24" ""
  , ppHiddenNoWindows = color "#555" ""
  , ppUrgent          = color "#ff3311" ""
  , ppLayout          = color "#65cf24" ""
  , ppSep             = " | "
  , ppWsSep           = " "
  }
  where color = xmobarColor

myLayoutHook = tiled ||| reflectHoriz tiled ||| Mirror tiled ||| Full
  where tiled =
          ResizableTall
          {- default number of windows in master pane -} 1
          {- resize increment    [fraction of screen] -} 0.02
          {- size of master pane [fraction of screen] -} 0.5
          []

myManageHook =
  composeAll
  [ className =? "Xfce4-notifyd" --> doIgnore
  ]

myKeys =
  [ ("M-S-<Delete>", io exitSuccess)
  ]

myDisabledKeys =
  [ "M-S-q"
  ]

myToggleStrutsKey conf = (modMask conf, xK_b)

-- | Prevent new windows from stealing focus.
avoidFocusStealing = doF focusDown
