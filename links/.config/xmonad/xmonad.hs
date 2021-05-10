import System.Environment (getEnv)

import XMonad
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (isDialog)

import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook, ewmhFullscreen)
import XMonad.Hooks.StatusBar (statusBarProp, withSB)
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.MultiToggle (Toggle (..), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
import XMonad.Layout.Reflect (reflectHoriz, reflectVert)
import qualified XMonad.StackSet as W
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.Hacks (javaHack)


igSB = statusBarProp "xmobar" (clickablePP igXmobarPP)
main :: IO ()
main = do
  igTerm <- getEnv "MYTERM"
  xmonad
     $ withSB igSB
     $ ewmhFullscreen
     $ ewmh
     $ docks
     $ javaHack (igConfig igTerm)

igConfig igTerm = def
    { terminal = igTerm
    , modMask    = mod4Mask
    , layoutHook = igLayout
    , manageHook = igManageHook <+> manageDocks
    }
  `additionalKeysP`
    [ ("M-<Return>", spawn igTerm)
    , ("M-r", spawn "rofiLauncher")
    , ("M-e", spawn "tvifm")
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-b", sendMessage ToggleStruts)
    , ("M-x", sendMessage $ Toggle NBFULL)
    ]

igManageHook :: ManageHook
igManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

igLayout = mkToggle (single NBFULL) $ avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = reflectHoriz $ reflectVert $ Tall nmaster delta ratio
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

igXmobarPP :: PP
igXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitle           = wrap (white    "[") (white    "]") . magenta . ppWindow
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    }
  where
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
