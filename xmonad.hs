module Main where

import           Control.Monad              (when)
import           Data.Monoid                (All (..))
import           System.IO

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks   (avoidStruts, manageDocks)
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import           XMonad.Layout.NoBorders    (smartBorders)
import           XMonad.Layout.Spacing      (smartSpacing)
import qualified XMonad.StackSet            as W
import           XMonad.Util.EZConfig       (additionalKeys)
import           XMonad.Util.Run            (spawnPipe)

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ def
        { manageHook = composeAll [ manageDocks
                                  , isFullscreen --> doFullFloat
                                  -- , className =? "Vlc" --> doFullFloat
                                  , manageHook def ]
        , handleEventHook = evHook
        , layoutHook = avoidStruts . smartBorders . smartSpacing 4 $ layoutHook def
        , logHook = dynamicLogWithPP xmobarPP
          { ppOutput  = hPutStrLn xmproc
          , ppTitle   = xmobarColor "#7cafc2" "" . shorten 50
          , ppCurrent = xmobarColor "#f7ca88" "" . wrap "[" "]"
            -- , ppLayout  = const ""
          , ppLayout = \layStr -> let ls = words layStr
                                  in unwords $ if length ls > 2
                                               then (tail . tail) ls
                                               else ls
          }
        , terminal = "urxvt"
        , modMask = mod4Mask
        , focusFollowsMouse = False
        , borderWidth        = 4
        , normalBorderColor  = "#181818"
        , focusedBorderColor = "#7cafc2"  -- "#a1b56c"
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn
           "xscreensaver-command -lock; xset dpms force off")
        , ((controlMask, xK_Print),  spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print),            spawn "scrot")
        , ((controlMask, xK_Return), spawn "dmenu_run")
        , ((0, xK_F1),               spawn "amixer -D pulse sset Master toggle")
        , ((0, xK_F2),               spawn "amixer -D pulse sset Master 5%-")
        , ((0, xK_F3),               spawn "amixer -D pulse sset Master 5%+")
        , ((0, xK_F5),               spawn "xbacklight -dec 10")
        , ((0, xK_F6),               spawn "xbacklight -inc 10")
        ]

evHook :: Event -> X All
evHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
    st     <- getAtom "_NET_WM_STATE"
    fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
    isFull <- runQuery isFullscreen win
    let remove = 0
        add    = 1
        toggle = 2
        ptype  = 4
        action = head dat
    when (typ == st && fromIntegral fullsc `elem` tail dat) $ do
        when (action == add || (action == toggle && not isFull)) $ do
            io $ changeProperty32 dpy win st ptype propModeReplace [fromIntegral fullsc]
            fullFloat win
        when (head dat == remove || (action == toggle && isFull)) $ do
            io $ changeProperty32 dpy win st ptype propModeReplace []
            tileWin win
    return $ All False
evHook _ = return $ All True

fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w (W.RationalRect 0 0 1 1)
tileWin w = windows $ W.sink w
