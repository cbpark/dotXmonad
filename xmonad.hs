module Main where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops  (ewmh)
import XMonad.Hooks.ManageDocks   (avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Layout.Fullscreen   (fullscreenSupport)
import XMonad.Layout.NoBorders    (smartBorders)
import XMonad.Layout.Spacing      (smartSpacing)
import XMonad.Util.EZConfig       (additionalKeys)
import XMonad.Util.Run            (spawnPipe)

import System.IO                  (Handle, hPutStrLn)

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ fullscreenSupport $ ewmh def
        { manageHook = composeAll [ manageDocks
                                  , isFullscreen --> doFullFloat
                                  , manageHook def
                                  ]
        , handleEventHook = docksEventHook
        , layoutHook = avoidStruts $ smartBorders . smartSpacing 10 $
                       layoutHook def
        , logHook = myLogHook xmproc
        , terminal = "urxvt"
        , modMask = mod4Mask
        , focusFollowsMouse = False
        , borderWidth = 5
        , normalBorderColor  = "#1f1f1f"
        , focusedBorderColor = "#6ca0a3"
        } `additionalKeys` myKeys

myLogHook :: Handle -> X ()
myLogHook proc =
    dynamicLogWithPP xmobarPP
    { ppOutput  = hPutStrLn proc
    , ppTitle   = xmobarColor "#6ca0a3" "" . shorten 50
    , ppCurrent = xmobarColor "#d0bf8f" "" . wrap "[" "]"
    , ppLayout  = \str -> let ls = words str
                          in unwords (if length ls > 2
                                      then (tail . tail) ls
                                      else ls)
    }

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
    [ ((mod4Mask .|. shiftMask, xK_z),
       spawn "xscreensaver-command -lock; xset dpms force off")
    , ((controlMask, xK_Print),  spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print),            spawn "scrot")
    , ((controlMask, xK_Return), spawn "dmenu_run")
    , ((0, xK_F1),               spawn "amixer -D pulse sset Master toggle")
    , ((0, xK_F2),               spawn "amixer -D pulse sset Master 5%-")
    , ((0, xK_F3),               spawn "amixer -D pulse sset Master 5%+")
    , ((0, xK_F5),               spawn "xbacklight -dec 10")
    , ((0, xK_F6),               spawn "xbacklight -inc 10")
    , ((0, xK_F7),               spawn "kb-light.py -")
    , ((0, xK_F8),               spawn "kb-light.py +")
    ]
