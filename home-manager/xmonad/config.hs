import Data.Char (toUpper)
import Data.Map qualified as M
import System.Environment
import System.IO (hClose)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Accordion (Accordion (Accordion))
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.MagicFocus
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.Paste (pasteString)
import XMonad.Util.Run (hPutStr, spawnPipe)
import XMonad.Util.Ungrab (unGrab)

-- Variables
myFont :: String
myFont = "xft:Mononoki Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

appLauncher :: String
appLauncher = "rofi -modi drun,ssh,window -show drun -show-icons"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "

myTerminal :: String
myTerminal = "kitty"

-- Startup hook. Mostly not used as nixos handles services pretty well.
myStartupHook :: X ()
myStartupHook = do
  spawn "autorandr default"

-- The keybindings with names so we can show with super - m
myKeys c =
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks
   in subKeys
        "Xmonad Essentials"
        [ ("M-<Return>", addName "terminal" $ spawn myTerminal)
        , ("M-S-r", addName "Restart XMonad" $ spawn "xmonad --restart")
        , ("M-d", addName "rofi" $ spawn appLauncher)
        , ("M-C-s", addName "screen shot" $ unGrab *> spawn "scrot -s")
        , -- Switch to single screen mode

          ( "M-s 1"
          , addName "1 screen mode" $ spawn "autorandr default"
          )
        , -- Switch to dual screen mode

          ( "M-s 2"
          , addName "2 screen mode" $ spawn "autorandr work"
          )
        , -- TODO change to use emacs daemon
          ("M-e", addName "org capture" $ spawn $ myTerminal ++ " org-capture")
        , ("M-y", addName "swap screen" nextScreen)
        , ("M-S-y", addName "swap window to screen" shiftNextScreen)
        ]

myLayout = tiled ||| Mirror tiled ||| Full ||| Grid ||| Accordion
 where
  tiled = Tall nmaster delta ratio
  nmaster = 1 -- default number of master panes
  ratio = 1 / 2 -- default proportion of screen occupied by master pane
  delta = 3 / 100 -- percent of screen to increment when resizing panes

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " ⟐ "
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

  -- \| Windows should have *some* title, which should not exceed a
  -- sane length.
  ppWindow :: String -> String
  ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

  -- TODO extract this to something more configurable
  blue, lowWhite, magenta, red, white, yellow :: String -> String
  magenta = xmobarColor "#ff79c6" ""
  blue = xmobarColor "#bd93f9" ""
  white = xmobarColor "#f8f8f2" ""
  yellow = xmobarColor "#f1fa8c" ""
  red = xmobarColor "#ff5555" ""
  lowWhite = xmobarColor "#bbbbbb" ""

-- Manage how windows open
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat
    , className =? "krita" --> doFloat
    , isDialog --> doFloat
    ]

-- show keybindings with subtitle
subtitle' :: String -> ((KeyMask, KeySym), NamedAction)
subtitle' x =
  ( (0, 0)
  , NamedAction $
      map toUpper $
        sep ++ "\n--- " ++ x ++ " ---\n" ++ sep
  )
 where
  sep = replicate (6 + length x) '-'

-- show keybindings in a dialog box (via yad)
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"Mononoki Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  hPutStr h (unlines $ showKmSimple x)
  hClose h
  return ()

myConfig =
  addDescrKeys'
    ((mod4Mask, xK_m), showKeybindings)
    myKeys
    gnomeConfig
      { modMask = mod4Mask -- Rebind Mod to the Super key
      , terminal = myTerminal
      , borderWidth = 1
      , normalBorderColor = "#bbbbbb"
      , focusedBorderColor = "#ff79c6"
      , layoutHook = myLayout
      , manageHook = myManageHook
      , startupHook = myStartupHook
      }

main :: IO ()
main = do
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig
