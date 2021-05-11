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


main :: IO ()
main = do
  igTerm <- getEnv "MYTERM"
  xmonad $ withSB igSB $ ewmhFullscreen $ ewmh $ docks
     $ javaHack $ igConfig igTerm

igConfig igTerm = def
    { terminal = igTerm
    , modMask    = mod4Mask
    , layoutHook = igLayout
    , manageHook = igManageHook <+> manageDocks
    , startupHook = igStartupHook
    }
  `additionalKeysP`
    [ ("M-<Return>", spawn igTerm)
    , ("M-r", spawn "rofiLauncher")
    , ("M-e", spawn "tvifm")
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-b", sendMessage ToggleStruts)
    , ("M-x", sendMessage $ Toggle NBFULL)
    ]

igStartupHook = do
    spawn "killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --height 17 --transparent true --alpha 0 --tint 0xfffffa --widthtype request --monitor 0"

igManageHook :: ManageHook
igManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , className =? "Xmessage" --> doFloat
    , isDialog            --> doFloat
    ]

igLayout = mkToggle (single NBFULL) $ avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled = reflectHoriz $ reflectVert $ Tall nmaster delta ratio
    nmaster = 1
    delta = 3 / 100
    ratio = 1 / 2

igSB = statusBarProp "xmobar" (clickablePP igXmobarPP)

igXmobarPP :: PP
igXmobarPP = def
    { ppSep             = " "
    , ppTitle           = wrap " " "" . magenta . ppWindow
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = blue
    , ppHidden          = white
    , ppVisibleNoWindows = Nothing
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \(ws:_:t:_) -> [ws,t]
    }
  where
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "" else w) . shorten 100

    blue, red, white, yellow :: String -> String
    magenta  = xmobarColor "#5c345f" ""
    blue     = xmobarColor "#00729b" ""
    white    = xmobarColor "#999999" ""
    yellow   = xmobarColor "#503d15" ""
    red      = xmobarColor "#ff0055" ""
