module Main where

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders     (toggleBorder)
import XMonad.Actions.NoBorders
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Config.Desktop        (desktopConfig)
import XMonad.Config.Prime          (sendMessage)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops    (ewmh, fullscreenEventHook)
import XMonad.Hooks.FadeInactive    (fadeIf, fadeOutLogHook, isUnfocused)
import XMonad.Hooks.ManageDocks     (ToggleStruts (..), avoidStruts,
                                     docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers   (doFullFloat, isFullscreen)
import XMonad.Layout.Hidden
import XMonad.Layout.NoBorders      (noBorders, smartBorders)
import XMonad.Layout.Spacing        (Border (..), spacingRaw)
import XMonad.Layout.Spiral         (spiral)
import XMonad.Util.EZConfig         (additionalKeysP, removeKeysP)
import XMonad.Util.Run              (spawnPipe)

import System.IO                    (Handle, hPutStrLn)

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
    xmonad $ ewmh desktopConfig
        { manageHook = myManageHook
        , handleEventHook = docksEventHook <+> fullscreenEventHook
        , layoutHook = avoidStruts $ smartBorders . spaces $ myLayout
        , logHook = myLogHook xmproc
        , terminal = "urxvt"
        , modMask = mod4Mask
        , focusFollowsMouse = False
        , borderWidth = 5
        , normalBorderColor  = "#1f1f1f" -- "#ffffef"
        , focusedBorderColor = "#cc9393"
        } `additionalKeysP` myKeybindings `removeKeysP` unusedKeys
  where
    spaces = spacingRaw True (Border 0 0 0 0) True (Border 10 10 10 10) True
    myLayout = let tall = Tall 1 (3/100) (1/2)
               in hiddenWindows (tall ||| Mirror tall ||| spiral (6/7) ||| noBorders Full)

myManageHook :: ManageHook
myManageHook = composeAll [ manageDocks
                          , className    =? "skype"       --> doFloat
                          , className    =? "gnome-mpv"   --> doFloat
                          , resource     =? "stalonetray" --> doIgnore
                          , isFullscreen                  --> doFullFloat
                          ]

myLogHook :: Handle -> X ()
myLogHook proc =
    dynamicLogWithPP xmobarPP
    { ppOutput  = hPutStrLn proc
    , ppTitle   = xmobarColor "#8cd0d3" "" . shorten 50
    , ppCurrent = xmobarColor "#f0dfaf" "" . wrap "[" "]"
    , ppLayout  = \str -> let ls = words str
                          in unwords (if length ls > 2 then drop 2 ls else ls)
    }
    -- move the pointer to the focused window
    >> updatePointer (0.5, 0.5) (1, 1)
    -- make inactive windows translucent
    >> (fadeOutLogHook . fadeIf isUnfocused) 0.9

myKeybindings :: [(String, X())]
myKeybindings =
    [ ("M-S-z",
       spawn "xscreensaver-command -lock; sleep 1; xset dpms force off")
    , ("M-S-e",                   spawn "ec")
    , ("M-S-f",                   spawn "firefox || chromium")
    , ("M-S-t",                   spawn "~/.xmonad/bin/toggle_composite.sh")
    , ("C-<Return>",              spawn "rofi -show run")
    , ("C-<Print>",               spawn "scrot")
    , ("<XF86AudioLowerVolume>",  spawn "amixer -D pulse sset Master 5%-")
    , ("<XF86AudioRaiseVolume>",  spawn "amixer -D pulse sset Master 5%+")
    , ("<XF86AudioMute>",         spawn "amixer -D pulse sset Master toggle")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
    , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 10")
    , ("M-<Down>",                nextWS)
    , ("M-<Up>",                  prevWS)
    , ("M-S-<Down>",              shiftToNext >> nextWS)
    , ("M-S-<Up>",                shiftToPrev >> prevWS)
    , ("M-<Left>",                prevScreen)
    , ("M-<Right>",               nextScreen)
    , ("M-S-<Left>",              shiftNextScreen)
    , ("M-S-<Right>",             shiftPrevScreen)
    , ("M-z",                     toggleWS)
    , ("M-f",                     moveTo Next NonEmptyWS)
    , ("M-]",                     withFocused hideWindow)
    , ("M-[",                     popOldestHiddenWindow)
    , ("M-S-<Space>",             sendMessage ToggleStruts)
    , ("M-S-n",                   withFocused toggleBorder)
    ]

unusedKeys :: [String]
unusedKeys = ["M-p", "M-n"]
