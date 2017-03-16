module Main where

import XMonad
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops    (ewmh)
import XMonad.Hooks.FadeInactive    (fadeIf, fadeOutLogHook, isUnfocused)
import XMonad.Hooks.ManageDocks     (avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers   (doFullFloat, isFullscreen)
import XMonad.Layout.Fullscreen     (fullscreenSupport)
import XMonad.Layout.NoBorders      (smartBorders)
import XMonad.Layout.Spacing        (smartSpacing)
import XMonad.Util.EZConfig         (additionalKeys)
import XMonad.Util.Run              (spawnPipe)

import System.IO                    (Handle, hPutStrLn)

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ fullscreenSupport $ ewmh def
        { manageHook = myManageHook def
        , handleEventHook = docksEventHook
        , layoutHook = avoidStruts $
                       (smartBorders . smartSpacing 10) (layoutHook def)
        , logHook = myLogHook xmproc
        , terminal = "urxvt"
        , modMask = mod4Mask
        , focusFollowsMouse = False
        , borderWidth = 5
        , normalBorderColor  = "#1f1f1f"
        , focusedBorderColor = "#6ca0a3"
        } `additionalKeys` myKeys

myManageHook :: XConfig a -> ManageHook
myManageHook conf = composeAll [ manageDocks
                               , className =? "vlc" --> doFloat
                               , resource =? "stalonetray" --> doIgnore
                               , isFullscreen --> doFullFloat
                               , manageHook conf
                               ]

myLogHook :: Handle -> X ()
myLogHook proc =
    dynamicLogWithPP xmobarPP
    { ppOutput  = hPutStrLn proc
    , ppTitle   = xmobarColor "#6ca0a3" "" . shorten 50
    , ppCurrent = xmobarColor "#d0bf8f" "" . wrap "[" "]"
    , ppLayout  = \str -> let ls = words str
                          in unwords (if length ls > 2
                                      then (tail . tail) ls
                                      else ls) }
    -- move the pointer to the focused window
    >> updatePointer (0.5, 0.5) (1, 1)
    -- make inactive windows translucent
    >> (fadeOutLogHook . fadeIf isUnfocused) 0.9

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
    [ ((mod4Mask .|. shiftMask, xK_z),
       spawn "xscreensaver-command -lock; xset dpms force off")
    , ((mod4Mask .|. shiftMask, xK_t), spawn "toggle_composite.sh")
    , ((mod4Mask .|. shiftMask, xK_e), spawn "ec")
    , ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")
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
