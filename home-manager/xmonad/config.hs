import qualified Data.Map              as M
import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Util.EZConfig  (additionalKeysP)

appLauncher  = "rofi -modi drun,ssh,window -show drun -show-icons"

myKeys = [ ("M-<Return>", spawn "alacritty")
      ,( "M-d", spawn appLauncher)
      ]

main :: IO ()
main =
  xmonad $
    desktopConfig
      { modMask = mod4Mask, -- Rebind Mod to the Super key
        terminal = "alacritty",
        borderWidth = 1
      }
      `additionalKeysP` myKeys
