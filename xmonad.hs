-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config
 
import System.IO
import System.Exit
import XMonad                   hiding ( (|||), Connection )
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers hiding (padL)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeInactive
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Util.Run
import XMonad.Config.Xfce
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutScreens
import XMonad.Layout.WindowNavigation
import System.Dzen.Padding
import System.Dzen.Base
import XMonad.Actions.Navigation2D

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1:code","2:web","3:msg","4:terms","5:media","6:docs","7:textbooks","8:overflow","9:misc", ""]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"
 
myStatusBar = "dzen2 -e 'button2=;' -x '0' -y '0' -h '24' -w 2048 -ta '1' -fg '#FFFFFF' -bg '#1B1D1E' -fn 'Inconsolata-Regular:size=14'"
myOtherStatusBar = "dzen2 -e 'button2=;' -x '2048' -y '0' -h '24' -w 1536 -ta '1' -fg '#FFFFFF' -bg '#1B1D1E' -fn 'Inconsolata-Regular:size=14'"
myTrayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --width 384 --widthtype pixel --transparent true --tint 0x222222 --alpha 0 --height 24"
myConky = "conky"
myConkyBar = "conky -c ~/.conkybarrc"
xfcepanel = "xfce4-panel"
conkySplit = screenGo L False >> layoutSplitScreen 2 (TwoPane 0.7575 0.2425)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    
    -- launch file browser
    , ((modMask .|. controlMask, xK_Return), spawn "thunar")

    -- launch gmrun
    , ((modMask .|. controlMask, xK_l     ), spawn "xscreensaver-command -lock")

    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "export PATH=$HOME/bin:$PATH; dmenu_run")
 
    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    , ((modMask .|. controlMask, xK_f   ), sendMessage $ JumpToLayout "Full")
    , ((modMask .|. controlMask, xK_t   ), sendMessage $ JumpToLayout "ThreeCol")
    , ((modMask .|. controlMask, xK_b   ), sendMessage $ JumpToLayout "Tabbed Simplest")
    , ((modMask .|. controlMask, xK_Up), sendMessage $ Move U)
    , ((modMask .|. controlMask, xK_Down), sendMessage $ Move D)
    , ((modMask .|. controlMask, xK_Right), sendMessage $ Move R)
    , ((modMask .|. controlMask, xK_Left), sendMessage $ Move L)
    , ((modMask .|. controlMask, xK_space), conkySplit)
    , ((modMask .|. controlMask .|. shiftMask, xK_space), rescreen)
    , ((modMask .|. shiftMask, xK_b), sendMessage ToggleStruts)
 
    , ((controlMask, xK_Up), spawn "amixer -q set Master 1+ unmute")
    , ((controlMask, xK_Down), spawn "amixer -q set Master 1- unmute")
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io exitSuccess)
 
    -- Restart xmonad
    , ((modMask              , xK_q     ), rescreen >> spawnPipe "pkill trayer" >> spawnPipe "pkill conky" >> spawnPipe "pkill xfce4-panel" >> restart "xmonad" True)
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [ xK_e, xK_r, xK_w ] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
 
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
myTabConfig = defaultTheme {   activeBorderColor = "#7C7C7C"
                             , activeTextColor = "#CEFFAC"
                             , activeColor = "#000000"
                             , inactiveBorderColor = "#7C7C7C"
                             , inactiveTextColor = "#EEEEEE"
                             , inactiveColor = "#000000" }
myLayout = (avoidStruts . windowNavigation) (tiled ||| Mirror tiled ||| tabbed shrinkText myTabConfig ||| Full ||| spiral (6/7) ||| ThreeColMid 1 (3/100) (1/2) ||| (tabbed shrinkText myTabConfig  **|*** Mirror tiled2))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     tiled2  = Tall nmaster delta ratio2
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     ratio2  = 7/8
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
 
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
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Smplayer"       --> doFloat
    , className =? "Psx.real"       --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Galculator"     --> doFloat
    , resource  =? "Komodo_find2"   --> doFloat
    , resource  =? "compose"        --> doFloat
    , className =? "Plasma"         --> doFloat
    , className =? "Update"         --> doFloat
    , className =? "Xfce4-mixer"    --> doFloat
    , className =? "Xfce4-settings-manager" --> doFloat
    , className =? "Xfce4*"         --> doFloat
    , className =? "Xfce4-panel"         --> doFloat
    , title     =? "Downloads"      --> doFloat
    , title     =? "About Aurora"   --> doFloat
    , className =? "Thunar"         --> doFloat
    , className =? "Terminal"       --> doShift "4:terms"
    , className =? "Gedit"          --> doShift "1:code"
    , className =? "Emacs"          --> doShift "1:code"
    , className =? "Komodo Edit"    --> doShift "1:code"
    , className =? "Emacs"          --> doShift "1:code"
    , className =? "Gvim"           --> doShift "1:code"
    , className =? "Aurora"         --> doShift "2:web"
    , className =? "Thunderbird-bin" --> doShift "3:msg"
    , className =? "Pidgin"         --> doShift "3:msg"
    , className =? "VirtualBox"     --> doShift "9:misc"
    , className =? "banshee-1"      --> doShift "5:media"
    , className =? "spotify"        --> doShift "5:media"
    , className =? "Ktorrent"       --> doShift "5:media"
    , className =? "Xchat"          --> doShift "5:media"
    , className =? "Conky"          --> doShift ""
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , resource  =? "steam"          --> doIgnore
    , className  =? "Firefox"    --> doShift "2:web"
    , isFullscreen                  --> (doF W.focusDown <+> doFullFloat) 
    ] 
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 

prettyPrinter ::  Int -> Handle -> PP
prettyPrinter padSize h = defaultPP { 
    ppOutput = hPutStrLn h
    , ppTitle = dzenColor "green" "black" . toString . padC padSize . str . shorten padSize
    , ppCurrent = dzenColor "green" "black" . wrap "[" "]" 
    , ppVisible = dzenColor "yellow" "black" . wrap "(" ")" 
    , ppHidden = dzenColor "orange" "black"
    , ppHiddenNoWindows = dzenColor "white" "black"
    , ppUrgent = dzenColor "red" "black"
    , ppLayout = dzenColor "green" "black"
    , ppOrder = \(ws:l:t:r) -> [t, ws, l] ++ r
    , ppExtras = [ date "%A %b %e" ]
    , ppSep = " | "
}


------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook n h = dynamicLogWithPP $ prettyPrinter n h
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = rescreen >> conkySplit >> setWMName "LG3D"
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.

main :: IO ()
main = do 
          dzenStatusBar <- spawnPipe myStatusBar
          dzenOtherStatusBar <- spawnPipe myOtherStatusBar
          conky <- spawnPipe myConky
          panel <- spawnPipe myTrayer
          xmonad $ defaults {
            logHook = myLogHook 100 dzenStatusBar >> myLogHook 60 dzenOtherStatusBar >> fadeInactiveLogHook 0xdddddddd
          }

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
defaults = xfceConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = avoidStruts . smartBorders $ myLayout,
        manageHook         = manageDocks <+> insertPosition Above Newer <+> myManageHook ,
        startupHook        = myStartupHook 
    }

------------------------------------------------------------------------

