import XMonad
import XMonad.Hooks.DynamicLog
import qualified Data.Map as Map
-- import System.IO

main = xmonad =<< statusBar "xmobar" myPP toggleStatusKey myConfig

myConfig = defaultConfig
  { modMask = mod4Mask -- use Super instead of Alt
  , terminal = "urxvt"
  , borderWidth = 3
  , normalBorderColor = "black"
  , focusedBorderColor = "#33ff7b"
  , keys = \ x -> Map.union (Map.fromList $ newKeys x) (keys defaultConfig x)
  }

myPP = defaultPP
  { ppCurrent = wrapFc "#ffff00"
  }

toggleStatusKey XConfig { modMask = modMask } = (modMask, xK_b)

wrapFc fc = wrap ("<fc=" ++ fc ++ ">") "</fc>"

-- Add new and/or redefine key bindings
newKeys (XConfig { modMask = modMask }) =
  [ ((modMask, xK_q), spawn "killall xmobar; xmonad --recompile; xmonad --restart")
  ]
