--
-- XMonad IMPORTS
--
import XMonad

import System.Exit
import Data.Char
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Util.Run
import XMonad.Util.Dmenu
import XMonad.Util.NamedScratchpad

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.WorkspaceHistory

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.LayoutScreens
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.Renamed

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map as M

--
-- XMonad Configs
--
myTerminal :: String
myTerminal      = "alacritty" -- Change this to the terminal you like

myModMask :: KeyMask
myModMask       = mod4Mask -- I think you should use mod2 on mac keyboards

myFont :: String
myFont =        "xft:Source Code Pro:regular:size=9:antialias=true:hinting=true"

myBorderWidth :: Dimension
myBorderWidth   = 2 -- Size in pixel

myNormalBorderColor :: String
myNormalBorderColor  = "#5e81ac"

myFocusedBorderColor :: String
myFocusedBorderColor = "#88c0d0"

-- MOUSE SUPPORT!!!!
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Applications
myBrowser :: String
myBrowser       = "firefox"

myEditor :: String
myEditor        = myTerminal ++ " -e nvim"

myRofi :: String
myRofi            = "rofi -modi window,drun,ssh -theme nord -show drun"

-- Workspaces
myWorkspaces    = ["web", "dev", "doc", "chat", "audio", "model", "gfx"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)
-- XMobar
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myBar = "xmobar"

myPP = xmobarPP 
            { ppCurrent = xmobarColor "#8fbcbb" "" . wrap "[ " " ]"         -- Current workspace
              , ppVisible = xmobarColor "#4c566a" "" . clickable              -- Visible but not current workspace
              , ppHidden = xmobarColor "#88c0d0" "" . wrap "" "*" . clickable -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#88c0d0" ""  . clickable     -- Hidden workspaces (no windows)
              , ppTitle = xmobarColor "#5e81ac" "" . shorten 60               -- Title of active window
              , ppSep =  "<fc=#81a1c1> <fn=1>|</fn> </fc>"                    -- Separator character
              , ppUrgent = xmobarColor "#ebcb8b" "" . wrap "!" "!"            -- Urgent workspace
              , ppExtras  = [windowCount]                                     -- # of windows current workspace
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]     
            }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
-- 
-- Startup Hooks
--

myStartupHook = do
  spawnOnce "feh --bg-scale $HOME/Pictures/wallpaper.jpg"
  spawnOnce "picom --experimental-backend"
  spawnOnce "dunst"
  spawnOnce "setxkbmap -option grp:alt_shift_toggle us,ir"
  spawnOnce "pactl get-sink-volume @DEFAULT_SINK@"
--  spawnOnce "xmobar"

--
-- Hooks
--
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "confirm"         --> doCenterFloat
    , className =? "file_progress"   --> doCenterFloat
    , className =? "dialog"          --> doCenterFloat
    , className =? "download"        --> doCenterFloat
    , className =? "error"           --> doCenterFloat
    , className =? "MPlayer"         --> doCenterFloat
    , className =? "Gimp"            --> doCenterFloat
    , resource  =? "desktop_window"  --> doIgnore
    , resource  =? "kdesktop"        --> doIgnore
    , title     =? "Mozilla Firefox" --> doShift ( myWorkspaces !! 0 )
    , className =? "neovim"          --> doShift ( myWorkspaces !! 1 )
    , className =? "emacs"           --> doShift ( myWorkspaces !! 1 )
    , title     =? "LibreOffice Calc"    --> doShift ( myWorkspaces !! 2 )
    , className =? "discord"         --> doShift ( myWorkspaces !! 3 )
    , className =? "mpv"             --> doShift ( myWorkspaces !! 4 )
    , className =? "vlc"             --> doShift ( myWorkspaces !! 4 )
    , title     =? "audacity"        --> doShift ( myWorkspaces !! 4 )
    , title     =? "Blender"         --> doShift ( myWorkspaces !! 5 )
    , className =? "Gimp"            --> doShift ( myWorkspaces !! 6 )
    , isFullscreen --> doFullFloat
                                 ]

-- 
-- Key Bindings
--

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- launch a terminal
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  , ((modMask .|. shiftMask, xK_s),
     spawn myBrowser)

  , ((modMask .|. shiftMask, xK_b),
     spawn myEditor)
  
  , ((modMask .|. shiftMask, xK_f),
     spawn myRofi)

  -- Volume And Brightness
    , ((0,                    xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0,                    xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
    , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")

  -- Screen Shot 
  , ((0,                      xK_Print), spawn "xfce4-screenshooter -f")
  
  -- Gaps!
  , ((modMask .|. controlMask, xK_g), sendMessage $ ToggleGaps)               -- toggle all gaps
  , ((modMask .|. shiftMask, xK_g), sendMessage $ setGaps [(L,30), (R,30), (U,40), (D,60)]) -- reset the GapSpec
    
  , ((modMask .|. controlMask, xK_t), sendMessage $ IncGap 10 L)              -- increment the left-hand gap
  , ((modMask .|. shiftMask, xK_t     ), sendMessage $ DecGap 10 L)           -- decrement the left-hand gap
    
  , ((modMask .|. controlMask, xK_y), sendMessage $ IncGap 10 U)              -- increment the top gap
  , ((modMask .|. shiftMask, xK_y     ), sendMessage $ DecGap 10 U)           -- decrement the top gap
    
  , ((modMask .|. controlMask, xK_u), sendMessage $ IncGap 10 D)              -- increment the bottom gap
  , ((modMask .|. shiftMask, xK_u     ), sendMessage $ DecGap 10 D)           -- decrement the bottom gap

  , ((modMask .|. controlMask, xK_i), sendMessage $ IncGap 10 R)              -- increment the right-hand gap
  , ((modMask .|. shiftMask, xK_i     ), sendMessage $ DecGap 10 R)           -- decrement the right-hand gap

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

  -- Restart xmonad
  , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
  ]
  ++

  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

--
-- Mouse binding
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

--
-- Layouts
--

myLayout = avoidStruts(smartBorders(tiled ||| Mirror tiled ||| Full))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
      
      -- hooks, layouts
--        manageHook  = manageDocks <+> manageHook defaults,
--        logHook     = ewmhDesktopsLogHook,
--        handleEventHook = ewmhDesktopsEventHook,
        layoutHook  = gaps [(L,10), (R,10), (U,20), (D,20)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders $ myLayout,
        startupHook = ewmhDesktopsStartup <+>  myStartupHook
    }
