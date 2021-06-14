-- xmonad config used by Cassandra Comar
-- Author: Cassandra Comar
-- http://github.com/cassandracomar/dotxmonad
{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent
import Control.Monad
import Data.Default
import Data.List (stripPrefix)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Graphics.X11.Xinerama
import System.Exit
import System.IO
import XMonad hiding (Connection, (|||))
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Xfce
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "emacsclient -c -e '(aweshell-new)'"

-- Width of the window border in pixels.
--
myBorderWidth = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", ""]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor = "#3c2c1c"

myFocusedBorderColor = "#eedece"

myFont = "xft:Inconsolata for Powerline:Medium:size=9"

background = "#181512"

foreground = "#D6C3B6"

myStatusBar sid horiz = iconsBar ++ trayer ++ infoBar
  where
    iconsBar = "~/.xmonad/status_bar '" ++ foreground ++ "' '" ++ background ++ "' '" ++ myFont ++ "' '" ++ show (horiz + 2823)
    trayer = "' &trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --width 150 --widthtype request --transparent true --tint '" ++ background ++ "' --alpha 0 --height 40 --monitor '" ++ show sid
    infoBar = "' &dzen2 -dock -e 'button2=;' -x '" ++ show horiz ++ "' -y 0 -h 40 -w 2823 -ta 'l' -fg '" ++ foreground ++ "' -bg '" ++ background ++ "' -fn '" ++ myFont ++ "'"

windowSpacing = 20

fixMonitor = "~/.files/monitor-hotplug.sh"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@XConfig {XMonad.modMask = modMask} =
  M.fromList $
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),
      ((modMask .|. controlMask, xK_l), spawn "xscreensaver-command -lock"),
      -- launch dmenu
      ((modMask, xK_p), spawn "CMD=$(yeganesh -x); dunstify $CMD; PATH=~/.local/bin/:$PATH $CMD"),
      -- suspend the computer
      ((modMask, xK_s), spawn "sudo systemctl suspend"),
      -- close focused window
      ((modMask .|. shiftMask, xK_c), kill),
      -- Rotate through the available layout algorithms
      ((modMask, xK_space), sendMessage NextLayout),
      ((modMask, xK_b), sendMessage ToggleStruts),
      --  Reset the layouts on the current workspace to default
      ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modMask, xK_n), refresh),
      -- Move focus to the next window
      ((modMask, xK_Tab), windows W.focusDown),
      -- Move focus to the next window
      ((modMask, xK_j), windows W.focusDown),
      -- Move focus to the previous window
      ((modMask, xK_k), windows W.focusUp),
      -- Move focus to the master window
      ((modMask, xK_m), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((modMask, xK_Return), windows W.swapMaster),
      -- Swap the focused window with the next window
      ((modMask .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((modMask .|. shiftMask, xK_k), windows W.swapUp),
      -- Shrink the master area
      ((modMask, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((modMask, xK_l), sendMessage Expand),
      -- Push window back into tiling
      ((modMask, xK_t), withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area
      ((modMask, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modMask, xK_period), sendMessage (IncMasterN (-1))),
      ((modMask .|. controlMask, xK_b), sendMessage $ JumpToLayout "IM Tabbed Simplest"),
      ((modMask .|. controlMask, xK_Up), sendMessage $ Move U),
      ((modMask .|. controlMask, xK_Down), sendMessage $ Move D),
      ((modMask .|. controlMask, xK_Right), sendMessage $ Move R),
      ((modMask .|. controlMask, xK_Left), sendMessage $ Move L),
      ((modMask .|. controlMask .|. shiftMask, xK_space), rescreen),
      ((modMask .|. shiftMask, xK_b), sendMessage ToggleStruts),
      ((mod4Mask, xK_Up), spawn "volume-adj.sh up"),
      ((mod4Mask, xK_Down), spawn "volume-adj.sh down"),
      ((mod4Mask .|. shiftMask, xK_space), spawn "volume-adj.sh mute"),
      ((mod4Mask, xK_space), spawn "mpc toggle"),
      ((mod4Mask, xK_Right), spawn "mpc next"),
      ((mod4Mask, xK_Left), spawn "mpc prev"),
      ((mod4Mask .|. shiftMask, xK_Right), spawn "playerctl position 5+"),
      ((mod4Mask .|. shiftMask, xK_Left), spawn "playerctl position 5-"),
      ((mod4Mask, xK_Return), spawn "/home/cassandra/.files/music"),
      ((mod4Mask, xK_b), spawn "/home/cassandra/.files/launch-dwb"),
      ((modMask, xK_m), spawn "mpdmenu"),
      ((mod4Mask, xK_f), spawn "sakura -t 'FILES' -x '/usr/bin/env ranger'"),
      ((mod4Mask, xK_i), spawn "sakura -t 'IRC' -x '/home/cassandra/.files/connect-irssi'"),
      -- Quit xmonad
      ((modMask .|. shiftMask, xK_q), io exitSuccess),
      -- Restart xmonad
      ((modMask, xK_q), rescreen >> restart "xmonad" True)
    ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0]),
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [ ((mask .|. modMask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
          (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
      ]

--    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [ xK_r, xK_w, xK_e ] [0..]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modMask} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w),
      -- mod-button2, Raise the window to the top of the stack
      ((modMask, button2), \w -> focus w >> windows W.swapMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myTabConfig =
  def
    { activeBorderColor = myFocusedBorderColor,
      activeTextColor = foreground,
      activeColor = background,
      inactiveBorderColor = myNormalBorderColor,
      inactiveTextColor = "#a39383",
      inactiveColor = background,
      fontName = myFont,
      decoHeight = 40
    }

myLayout = avoidStruts . modifySpecifics $ tabbedLayout ||| spacedLayout ||| codingLayout
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    tiled2 = Tall nmaster delta ratio2

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    ratio2 = 7 / 8

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

    -- IM Pane size
    imratio = 3 / 16
    withMyIM = withIM imratio (Title "Buddy List")

    -- Layout pieces
    uniformBorder i = Border i i i i
    spacedLayout = spacingRaw True (uniformBorder 0) False (uniformBorder windowSpacing) True . windowNavigation $ tiled
    tabbedLayout = windowNavigation $ tabbed shrinkText myTabConfig
    codingLayout = windowNavigation $ Mirror tiled2 ****|*** tabbed shrinkText myTabConfig

    -- Workspace-specific layouts.
    imWorkspace = onWorkspace "III" $ withMyIM tabbedLayout
    codingWorkspace = onWorkspace "I" $ tabbedLayout ||| codingLayout
    terminalWorkspace = onWorkspace "IV" spacedLayout
    modifySpecifics = imWorkspace . codingWorkspace . terminalWorkspace

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing

-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

myManageHook =
  composeAll
    [ className =? "Psx.real" --> doFloat,
      className =? "feh" --> doFloat,
      title =? "About Aurora" --> doFloat,
      title =? "Software Update" --> doFloat,
      title =? "Rename Bookmark Item" --> doRectFloat (W.RationalRect (3 / 8) (7 / 16) (1 / 4) (1 / 8)),
      title =? "MUSIC" --> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)),
      title =? "FILES" --> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)),
      title =? "Print" --> doRectFloat (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)),
      title =? "IRC" --> doShift "III",
      title =? "Signal" --> doShift "III",
      className =? "Gvim" --> doShift "I",
      className =? "firefox" --> doShift "II",
      className =? "Clementine" --> doShift "V",
      className =? "Ario" --> doShift "V",
      className =? "VirtualBox" --> doShift "IX",
      className =? "Slack" --> doShift "III",
      className =? "Riot" --> doShift "III",
      className =? "discord" --> doShift "III",
      className =? "Signal" --> doShift "III",
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore,
      resource =? "steam" --> doIgnore,
      resource =? "discord" --> doShift "III",
      resource =? "signal" --> doShift "III",
      title =? "helm" --> doFloat,
      isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

wrapWorkspaceClickable :: String -> String
wrapWorkspaceClickable s = wrap ("^ca(1, xdotool key alt+" ++ show (romanToInt s) ++ ")") "^ca()" s

prettyPrinter :: PP
prettyPrinter =
  def
    { ppTitle = dzenColor foreground background,
      ppCurrent = dzenColor "#f5e3d5" background . wrapWorkspaceClickable,
      ppVisible = dzenColor "#d4c4b4" background . wrapWorkspaceClickable,
      ppHidden = dzenColor "#a39383" background . wrapWorkspaceClickable,
      ppHiddenNoWindows = dzenColor "#716151" background . wrapWorkspaceClickable,
      ppUrgent = dzenColor "#D23D3D" background . wrapWorkspaceClickable,
      ppLayout = dzenColor foreground background . fixLayoutName,
      ppOrder = id,
      ppSep = " | "
    }

romanToInt :: String -> Int
romanToInt = fst . foldr ((\p (t, s) -> if p >= s then (t + p, p) else (t - p, p)) . fromJust . flip lookup (zip "IVXLCDM" [1, 5, 10, 50, 100, 500, 1000])) (0, 0)

fixLayoutName :: String -> String
fixLayoutName (stripPrefix "IM " -> Just s) = fixLayoutName s
fixLayoutName (stripPrefix "SmartSpacing 20 " -> Just s) = fixLayoutName s
fixLayoutName "Tabbed Simplest" = "Tabbed"
fixLayoutName "combining Mirror Tall and Tabbed Simplest with Tall" = "Coding"
fixLayoutName s = s

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
statusBars :: ScreenId -> IO Handle
statusBars sid' = do
  let sid = fromInteger $ toInteger sid'
  dpy <- openDisplay ""
  rects <- getScreenInfo dpy
  let x0 = rect_x $ rects !! sid

  handle <- spawnPipe $ myStatusBar sid' x0
  closeDisplay dpy
  return handle

myLogHook :: X ()
myLogHook = multiPP prettyPrinter prettyPrinter

myCleanUpHook :: IO ()
myCleanUpHook = return () --spawn "pkill dzen2"

myEventHook = dynStatusBarEventHook statusBars myCleanUpHook

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawn fixMonitor
  setWMName "LG3D"
  dynStatusBarStartup statusBars myCleanUpHook

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.

main :: IO ()
main = xmonad myXMConfig

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
myXMConfig =
  docks $
    xfceConfig
      { -- simple stuff
        terminal = myTerminal,
        focusFollowsMouse = myFocusFollowsMouse,
        borderWidth = myBorderWidth,
        modMask = myModMask,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        -- key bindings
        keys = myKeys,
        mouseBindings = myMouseBindings,
        -- hooks, layouts
        layoutHook = avoidStruts . smartBorders $ myLayout,
        manageHook = manageDocks <+> insertPosition Above Newer <+> myManageHook,
        startupHook = docksStartupHook >> myStartupHook,
        handleEventHook = docksEventHook >> myEventHook,
        logHook = myLogHook >> fadeInactiveLogHook 0xdddddddd
      }
      `additionalKeysP` [ ("<XF86MonBrightnessDown>", spawn "~/.files/backlight-adj.sh down"),
                          ("<XF86MonBrightnessUp>", spawn "~/.files/backlight-adj.sh up"),
                          ("<XF86AudioMute>", spawn "~/.files/volume-adj.sh mute"),
                          ("<XF86AudioLowerVolume>", spawn "~/.files/volume-adj.sh down"),
                          ("<XF86AudioRaiseVolume>", spawn "~/.files/volume-adj.sh up"),
                          ("<XF86AudioPrev>", spawn "playerctl previous"),
                          ("<XF86AudioPlay>", spawn "playerctl play-pause"),
                          ("<XF86AudioNext>", spawn "playerctl next")
                        ]

------------------------------------------------------------------------
