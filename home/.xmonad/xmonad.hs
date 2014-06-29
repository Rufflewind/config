{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Printf
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import qualified Data.Map as Map

main = do
  dzen <- spawnPipe myBar
  spawn myBarC
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
    , manageHook = manageDocks <+> manageHook defaultConfig
    }

barFg = "#aaaaaa"
barBg = "#222222"
barFont = "Envy Code R"
barHeight = 16

myBar = printf "dzen2 -ta l -fg '%s' -bg '%s' -fn '%s' -w 1440 -h %d"
        barFg barBg barFont (barHeight :: Int)

myBarC = printf "conky | dzen2 -ta r -fg '%s' -bg '%s' -fn '%s' -x 1440 -w 480 -h %d"
         barFg barBg barFont (barHeight :: Int)

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
