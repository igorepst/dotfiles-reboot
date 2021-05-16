{-# LANGUAGE FlexibleContexts #-}
import System.Environment (getEnv)
import Text.Regex.Posix ((=~))

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
    , workspaces = igWorkspaces
    }
  `additionalKeysP`
    [ ("M-<Return>", spawn igTerm)
    , ("M-r", spawn "rofiLauncher")
    , ("M-e", spawn "tvifm")
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-b", sendMessage ToggleStruts)
    , ("M-x", sendMessage $ Toggle NBFULL)
    , ("M-S-/", igHelpCommand) 
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10") 
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10") 
    ]
    where
    igHelpCommand :: X ()
    igHelpCommand = spawn ("echo -e " ++ show igHelp ++ " | xmessage -file -")

igWorkspaces :: [WorkspaceId]
-- , , , 
igWorkspaces = ["\xf8a4", "\xf8a7", "\xf8aa", "\xf8ad" ]

-- Regular expressions matching for ManageHooks
(~?) :: (Functor f) => f String -> String -> f Bool
q ~? x = fmap (=~ x) q

igManageHook :: ManageHook
igManageHook = composeAll
    [ className ~? "Gimp" --> doFloat
    , className =? "Xmessage" --> doFloat
    , className =? "MPlayer" --> doFloat
    , className =? "mpv" --> doFloat
    , className =? "zoom" --> doFloat
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


igHelp :: String
igHelp = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Enter        Launch terminal",
    "mod-r            Launch rofi",
    "mod-e            Launch file manager",
    "mod-b            Toggle struts",
    "mod-x            Toggle full layout",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Shift-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "",
    "-- Workspaces & screens",
    "mod-[1..9]         Switch to workSpace N",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
