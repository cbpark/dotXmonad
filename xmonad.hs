module Main where

import XMonad
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Config.Desktop        (desktopConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops    (ewmh)
import XMonad.Hooks.FadeInactive    (fadeIf, fadeOutLogHook, isUnfocused)
import XMonad.Hooks.ManageDocks     (avoidStruts, docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers   (doFullFloat, isFullscreen)
import XMonad.Layout.Fullscreen     (fullscreenSupport)
import XMonad.Layout.NoBorders      (smartBorders)
import XMonad.Layout.Spacing        (smartSpacing)
import XMonad.Layout.Spiral         (spiral)
import XMonad.Util.EZConfig         (additionalKeysP, removeKeysP)
import XMonad.Util.Run              (spawnPipe)

import System.IO                    (Handle, hPutStrLn)

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    xmonad $ fullscreenSupport $ ewmh desktopConfig
        { manageHook = myManageHook
        , handleEventHook = docksEventHook
        , layoutHook = avoidStruts $ (smartBorders . smartSpacing 10) myLayout
        , logHook = myLogHook xmproc
        , terminal = "urxvt"
        , modMask = myModMask
        , focusFollowsMouse = False
        , borderWidth = 4
        , normalBorderColor  = "#1d1f21"  -- "1f1f1f"
        , focusedBorderColor = "#c5c8c6"  -- "6ca0a3"
        } `additionalKeysP` myKeybindings `removeKeysP` unusedKeys
  where
    myLayout = let tall = Tall 1 (3/100) (1/2)
               in tall ||| Mirror tall ||| spiral (6/7) ||| Full

myManageHook :: ManageHook
myManageHook = composeAll [ manageDocks
                          , className    =? "skype"       --> doFloat
                          , className    =? "vlc"         --> doFloat
                          , resource     =? "stalonetray" --> doIgnore
                          , isFullscreen                  --> doFullFloat
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

myModMask :: KeyMask
myModMask = mod4Mask

myKeybindings :: [(String, X())]
myKeybindings =
    [ ("M-S-z",
       spawn "xscreensaver-command -lock; sleep 1; xset dpms force off")
    , ("M-S-e",                   spawn "ec")
    , ("M-S-f",                   spawn "firefox")
    , ("M-S-t",                   spawn "~/.xmonad/bin/toggle_composite.sh")
    , ("C-<Return>",              spawn "rofi -show run")
    , ("M-<Tab>",                 spawn "rofi -show window")
    , ("C-<Print>",               spawn "scrot")
    , ("<XF86AudioLowerVolume>",  spawn "amixer -D pulse sset Master 5%-")
    , ("<XF86AudioRaiseVolume>",  spawn "amixer -D pulse sset Master 5%+")
    , ("<XF86AudioMute>",         spawn "amixer -D pulse sset Master toggle")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
    , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 10")
    ]

unusedKeys :: [String]
unusedKeys = ["M-p", "M-n"]
