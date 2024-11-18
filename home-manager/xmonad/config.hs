import qualified Data.Map                   as M
import           System.Environment
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Config.Desktop
import           XMonad.Config.Gnome
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.Grid         (Grid (Grid))
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.Magnifier    (magnifiercz')
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig       (additionalKeysP)
import           XMonad.Util.Loggers
import           XMonad.Util.Paste          (pasteString)
import           XMonad.Util.Ungrab         (unGrab)


appLauncher = "rofi -modi drun,ssh,window -show drun -show-icons"
myKeys =
  [ ("M-<Return>", spawn "kitty")
  , ("M-d", spawn appLauncher)
  ,("M-C-s", unGrab *> spawn "scrot -s")
    -- Switch to single screen mode
  , ( "M-s 1",
       spawn "autorandr default")
  -- Switch to dual screen mode
  , ("M-s 2",
       spawn "autorandr work")
  , ("C-S-p", pasteString "psql postgres://$POSTGRES_USER:$POSTGRES_PASSWORD@localhost:5432/$POSTGRES_DATABASE?sslmode=disable")
  , ("M-e", spawn "kitty org-capture")
  , ("M-y", nextScreen)
  , ("M-S-y", shiftNextScreen)
  ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol ||| Grid
  where
    tiled = Tall nmaster delta ratio
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    nmaster = 1 -- default number of master panes
    ratio = 1/2 -- default proportion of screen occupied by master pane
    delta = 3/100 -- percent of screen to increment when resizing panes

myXmobarPP :: PP
myXmobarPP = def
  {
    ppSep = magenta " ⟐ "
  , ppTitleSanitize = xmobarStrip
  , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
  , ppHidden = white . wrap " " ""
  , ppHiddenNoWindows = lowWhite . wrap " " ""
  , ppUrgent = red . wrap (yellow "!") (yellow "!")
  , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
  , ppExtras = [logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- | Windows should have *some* title, which should not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue= xmobarColor "#bd93f9" ""
    white= xmobarColor "#f8f8f2" ""
    yellow= xmobarColor "#f1fa8c" ""
    red= xmobarColor "#ff5555" ""
    lowWhite= xmobarColor "#bbbbbb" ""

myConfig =
          gnomeConfig
            { modMask = mod4Mask, -- Rebind Mod to the Super key
              terminal = "kitty",
              borderWidth = 1,
              normalBorderColor = "#bbbbbb",
              focusedBorderColor = "#ff79c6",
              layoutHook = myLayout
            }
            `additionalKeysP` myKeys

main :: IO ()
main = do
        myEnvVar <- lookupEnv "BASE" >>= return . maybe "defaultValue" id
        xmonad .
          ewmhFullscreen .
          ewmh .
          withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey $ myConfig
