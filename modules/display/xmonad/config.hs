{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import Data.Char (toUpper)
import Data.Map qualified as M
import System.Environment ()
import System.IO (hClose)
import XMonad (
    Default (def),
    Full (Full),
    KeyMask,
    KeySym,
    ManageHook,
    Mirror (Mirror),
    Tall (Tall),
    X,
    XConfig (
        borderWidth,
        focusedBorderColor,
        layoutHook,
        manageHook,
        modMask,
        normalBorderColor,
        startupHook,
        terminal
    ),
    appName,
    className,
    composeAll,
    doFloat,
    doIgnore,
    io,
    mod4Mask,
    spawn,
    title,
    unGrab,
    xK_m,
    xmonad,
    (-->),
    (=?),
    (|||),
 )
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen)
import XMonad.Actions.GridSelect (def)
import XMonad.Actions.Prefix (PrefixArgument (Raw), usePrefixArgument, withPrefixArgument)
import XMonad.Config.Desktop ()
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Hooks.DynamicLog (
    PP (
        ppCurrent,
        ppExtras,
        ppHidden,
        ppHiddenNoWindows,
        ppOrder,
        ppSep,
        ppTitleSanitize,
        ppUrgent
    ),
    def,
    shorten,
    wrap,
    xmobarBorder,
    xmobarColor,
    xmobarRaw,
    xmobarStrip,
 )
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.StatusBar (
    defToggleStrutsKey,
    statusBarProp,
    withEasySB,
 )
import XMonad.Hooks.StatusBar.PP (
    PP (
        ppCurrent,
        ppExtras,
        ppHidden,
        ppHiddenNoWindows,
        ppOrder,
        ppSep,
        ppTitleSanitize,
        ppUrgent
    ),
    def,
    shorten,
    wrap,
    xmobarBorder,
    xmobarColor,
    xmobarRaw,
    xmobarStrip,
 )
import XMonad.Layout.Accordion (Accordion (Accordion))
import XMonad.Layout.Fullscreen (fullscreenFloat, fullscreenFull, fullscreenManageHook)
import XMonad.Layout.Grid (Grid (Grid))
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Layout.MagicFocus ()
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ThreeColumns ()
import XMonad.Prompt
import XMonad.Prompt.OrgMode (orgPrompt, orgPromptPrimary, orgPromptRefile)
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.NamedActions (
    NamedAction (..),
    addDescrKeys',
    addName,
    showKmSimple,
 )
import XMonad.Util.Paste (pasteString)
import XMonad.Util.Run (elispFun, execute, hPutStr, inEmacs, proc, spawnExternalProcess, spawnPipe, (>->))
import XMonad.Util.Scratchpad (scratchpadManageHookDefault)
import XMonad.Util.Ungrab (unGrab)

import Colors

-- Variables
-- TODO make this updateable like the colors
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
    spawn "autorandr default && "
    spawn "feh --bg-scale ~/.background-image"

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
              ("M-e", addName "org capture" emacsCapture)
            , ("M-y", addName "swap screen" nextScreen)
            , ("M-S-y", addName "swap window to screen" shiftNextScreen)
            ]

emacsCapture =
    withPrefixArgument $
        (`uncurry` orgSettings) . \case
            Raw _ -> orgPromptPrimary promptConfig
            _anything -> orgPromptRefile promptConfig
  where
    orgSettings = ("TODO", "~/org/todo.org")

promptConfig =
    def
        { font = "xft:Mononoki Nerd Font:pixelsize=32"
        , borderColor = color03
        , fgColor = colorFg
        , fgHLight = color02
        , bgColor = colorBg
        , bgHLight = color07
        , height = 60
        , position = Top
        }

myLayout = smartBorders $ layoutHints $ tiled ||| Mirror tiled ||| full ||| Grid ||| Accordion
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- default number of master panes
    ratio = 1 / 2 -- default proportion of screen occupied by master pane
    delta = 3 / 100 -- percent of screen to increment when resizing panes
    full = (fullscreenFloat . fullscreenFull) Full

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
    magenta = xmobarColor color06 ""
    blue = xmobarColor color05 ""
    white = xmobarColor color08 ""
    yellow = xmobarColor color04 ""
    red = xmobarColor color02 ""
    lowWhite = xmobarColor colorSecondary ""

myManageHook :: ManageHook
myManageHook =
    composeAll $
        [isDialog --> doFloat]
            ++ [appName =? r --> doIgnore | r <- myIgnores]
            ++
            -- auto-float certain windows
            [className =? c --> doCenterFloat | c <- myCenFloats]
            ++ [title =? t --> doFloat | t <- myFloat]
            ++
            -- fulscreen windows to fullfloating
            [isFullscreen --> doFullFloat]
            ++
            -- unmanage docks such as gnome-panel and dzen
            [ fullscreenManageHook
            , scratchpadManageHookDefault
            ]
  where
    -- windows to operate
    myIgnores =
        ["desktop", "kdesktop", "desktop_window", "stalonetray"]
    myCenFloats =
        ["Steam", "steam", "vlc", "Vlc", "mpv"]
    myFloat = ["Hangouts", "Gimp"]

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
    h <- spawnPipe "yad --text-info --fontname=\"Mononoki Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
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
            , normalBorderColor = color03
            , focusedBorderColor = color02
            , layoutHook = myLayout
            , manageHook = myManageHook
            , startupHook = myStartupHook
            }

main :: IO ()
main = do
    xmonad
        . spawnExternalProcess def
        . usePrefixArgument "M-u"
        . ewmhFullscreen
        . ewmh
        . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
        $ myConfig
