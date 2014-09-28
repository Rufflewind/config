{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Exception (bracket)
import Data.Monoid ((<>))
import Text.Printf (printf)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.StackSet (focusDown)
import XMonad.Util.Run
import qualified Graphics.X11.Xlib as X
import qualified Data.Map as Map

main :: IO ()
main = do

  (screenWidth, screenHeight) <- withDefaultDisplay $ \ display ->
    let screen = X.defaultScreen display in
    return (fromIntegral $ X.displayWidth  display screen,
            fromIntegral $ X.displayHeight display screen)
  dzen <- spawnBars screenWidth screenHeight

  xmonad $ defaultConfig
    { modMask = mod4Mask                -- use Super instead of Alt
    , terminal = "urxvt"
    , borderWidth = 3
    , normalBorderColor = "black"
    , focusedBorderColor = "#33ff7b"
    , keys = \ x -> Map.union (Map.fromList $ myKeys x)
                              (keys defaultConfig x)
    , logHook = myLogHook dzen
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , manageHook = -- avoidFocusStealing <>
                   manageDocks
                <> manageHook defaultConfig
    }

-- | Prevent new windows from stealing focus.
avoidFocusStealing = doF focusDown

withDefaultDisplay = bracket (X.openDisplay "") X.closeDisplay

spawnBars screenWidth screenHeight = do
  dzen <- spawnPipe myBar
  spawn myBarC
  return dzen
  where myBar      = printf ("dzen2 -ta l -fg '%s' -bg '%s' -fn '%s' " ++
                             "-w %d -h %d")
                            barFg barBg barFont conkyX barHeight
        myBarC     = printf ("conky | dzen2 -ta r -fg '%s' -bg '%s' " ++
                             "-fn '%s' -x %d -w %d -h %d")
                            barFg barBg barFont conkyX conkyWidth barHeight
        conkyX     = screenWidth - conkyWidth
        barFont    = "Envy Code R"
        barHeight  = 16  :: Int
        conkyWidth = 480 :: Int

barFg = "#aaaaaa"
barBg = "#222222"

myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor "#ffffff" barBg . pad
    , ppVisible         = dzenColor "#259f54" barBg . pad
    , ppHidden          = dzenColor "#557e84" barBg . pad
    , ppHiddenNoWindows = dzenColor "#444444" barBg . pad
    , ppUrgent          = dzenColor "#ff3311" barBg . pad
    , ppWsSep           = " "
    , ppSep             = "  |  "
    , ppLayout          = dzenColor "#259f54" barBg
    , ppTitle           = (" " ++) . dzenColor "white" barBg . dzenEscape
    , ppOutput          = hPutStrLn h
    }

myKeys (XConfig { modMask = modMask }) =
  [ ((modMask, xK_q),
     spawn "killall dzen2; xmonad --recompile; xmonad --restart")
  ]
