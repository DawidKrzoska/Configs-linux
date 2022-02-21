import System.IO
import System.Exit
import XMonad

  -- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.ManageDocks
import XMonad.ManageHook
import XMonad.Hooks.Minimize
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.ServerMode
import XMonad.Hooks.DynamicProperty
import XMonad.Layout.Gaps

  -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.Minimize
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.CenteredMaster(centerMaster)
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.IndependentScreens

  -- layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

  -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.MouseResize
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Actions.Minimize

  -- Utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

  -- Data
import Data.Maybe (isJust)
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import XMonad.Config.Desktop
import XMonad.Config.Azerty
import Control.Monad (liftM2)
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified DBus as D
import qualified DBus.Client as D

myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"
    spawnOnce "conky -c $HOME/.conky/miui/miui"

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

-- colours
normBord = "#282c34" -- Border color of normal windows
focdBord = "#4380e0" -- Border color of focused windows
fore     = "#DEE3E0"
back     = "#282c34"
winType  = "#c678dd"

myTerminal = "kitty" -- Sets default terminal

myEditor = myTerminal ++ " -e vim " -- Sets vim as editor for tree select

isAppRunning = "bash $HOME/.local/bin/is_app_running.sh"

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myModMask = mod4Mask   -- Sets modkey to super/windows key
encodeCChar = map fromIntegral . B.unpack
myFocusFollowsMouse = True
myBorderWidth = 2 -- Sets border width for windows

myWorkspaces    = [" Code "," Web "," Dsc ", " Mess "," Onote ", " Spot "," ClUp ", " vm " ,"vid"]

myBaseConfig = desktopConfig


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


-- window manipulations
myManageHook = composeAll . concat $
    [ --[isDialog --> doCenterFloat]
      [className =? c --> doCenterFloat | c <- myCFloats]
    , [isFullscreen --> doFullFloat]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces !! 1) | x <- www]
--    , [className =? "Spotify" --> doShift (myWorkspaces !! 4) ]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces !! 6) | x <- spot]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces !! 3) | x <- dsc]
    ]
    where
      doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
      myCFloats = ["Arandr", "Arcolinux-tweak-tool.py", "feh", "mpv", "Spotify Premium"]
      myTFloats = ["Downloads", "Save As..."]
      myRFloats = []
      myIgnores = ["desktop_window"]
      spot = ["Spotify Premium","Spotify"]
      dsc = ["discord"]

-- layouts
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining layouts
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 12
           $ ResizableTall 1 (3/100) (1/2) []
tall_no_spaces    = renamed [Replace "tall_no_spaces"]
           $ limitWindows 12
           $ mySpacing 3
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 4
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ mySpacing 10 
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ mySpacing' 4
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ mySpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabConfig
  where
    myTabConfig = def { fontName            = "xft:Mononoki Nerd Font:regular:pixelsize=13"
                      , activeColor         = "#24b4d1"
                      , inactiveColor       = "#3e445e"
                      , activeBorderColor   = "#292d3e"
                      , inactiveBorderColor = "#292d3e"
                      , activeTextColor     = "#000000"
                      , inactiveTextColor   = "#d0d0d0"
                      }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Sans:bold:size=60"
    , swn_fade              = 1
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#FFFFFF"
    }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT)  myDefaultLayout
             where
               -- I've commented out the layouts I don't use.
               myDefaultLayout =  minimize (tall_no_spaces)
                                 ||| minimize (tall)
                                 ||| spirals
                                 ||| grid
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| noBorders tabs

-- ScratchPads
-- myScratchPads :: [NamedScratchpad]
myScratchPads = [NS "scratchterminal" "kitty --name scratchterminal" (resource =? "scratchterminal" <||> title =? "scratchterminal")
        (customFloating $ W.RationalRect l t w h)	, NS "scratchhtop" "kitty -name scratchhtop -e htop" (resource =? "scratchhtop" <||> title =? "scratchhtop")
        (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5)
        , NS "floatingSpotify" "$HOME/.local/bin/is_spotify_running" (resource =? "floatingSpotify" <||> title =? "spotify")
        (customFloating $ W.RationalRect 0.2 0.4 0.6 0.4)]
     where
        h = 0.5
        w = 0.7
        t = 0
        l = (1 - w) / 2

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ --((modMask, 3), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    --, ((modMask, 2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    --, ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))

    ]


rofi_launcher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "

window_switcher = spawn "rofi -no-lazy-grab -show window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "
-- keys config

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS



  
  [ ((modMask, xK_e), spawn $ "emacs" )
  , ((modMask, xK_c), spawn $ "conky-toggle" )
  , ((modMask, xK_o), rofi_launcher)
  , ((modMask, xK_b), spawn $ "firefox" )
  , ((modMask, xK_w), window_switcher)
  , ((modMask, xK_f), sendMessage $ MT.Toggle NBFULL)
  , ((modMask, xK_h), spawn $ myTerminal ++ " -e htop" )
  , ((modMask, xK_q), kill )
  , ((modMask, xK_r), spawn $ myTerminal ++ " -e ranger" )
  , ((modMask, xK_t), spawn $ myTerminal )
  , ((modMask, xK_v), spawn $ "pavucontrol" )
  , ((modMask, xK_Escape), spawn $ "xkill" )
  , ((modMask, xK_Return), spawn $ myTerminal )
  , ((modMask, xK_F5), spawn $ "meld" )

  -- SUPER + SHIFT KEYS

  , ((modMask .|. shiftMask , xK_d ), spawn $ "dmenu_run -i -nb '#191919' -nf '#fea63c' -sb '#fea63c' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'")
  , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_q ), kill)
  , ((modMask .|. shiftMask , xK_x ), io (exitWith ExitSuccess))

  -- CONTROL + ALT KEYS


  , ((controlMask .|. mod1Mask , xK_b ), spawn $ "thunar")
  , ((controlMask .|. mod1Mask , xK_c ), spawn $ "caprine")
  , ((controlMask .|. mod1Mask , xK_f ), spawn $ "firefox")
  , ((controlMask .|. mod1Mask , xK_d ), spawn $ isAppRunning ++ " -a discord -p /opt/discord/Discord")
  , ((controlMask .|. mod1Mask , xK_i ), spawn $ "nitrogen")
  , ((controlMask .|. mod1Mask , xK_k ), spawn $ "arcolinux-logout")
  , ((controlMask .|. mod1Mask , xK_m ), spawn $ "xfce4-settings-manager")
  , ((controlMask .|. mod1Mask , xK_o ), spawn $ "$HOME/.xmonad/scripts/picom-toggle.sh")
  , ((controlMask .|. mod1Mask , xK_r ), spawn $ "rofi-theme-selector")
  , ((controlMask .|. mod1Mask , xK_s ), spawn $ "$HOME/.local/bin/is_spotify_running")
  , ((controlMask .|. mod1Mask , xK_u ), spawn $ "pavucontrol")

  -- ALT + ... KEYS

  , ((mod1Mask, xK_f), spawn $ "variety -f" )
  , ((mod1Mask, xK_n), spawn $ "variety -n" )
  , ((mod1Mask, xK_p), spawn $ "variety -p" )
  , ((mod1Mask, xK_r), spawn $ "xmonad --restart" )
  , ((mod1Mask, xK_t), spawn $ "variety -t" )
  , ((mod1Mask, xK_Up), spawn $ "variety --pause" )
  , ((mod1Mask, xK_Down), spawn $ "variety --resume" )
  , ((mod1Mask, xK_Left), spawn $ "variety -p" )
  , ((mod1Mask, xK_Right), spawn $ "variety -n" )
  , ((mod1Mask, xK_F2), spawn $ "rofi -show run" )

  --VARIETY KEYS WITH PYWAL

  , ((mod1Mask .|. shiftMask , xK_f ), spawn $ "variety -f && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_n ), spawn $ "variety -n && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_p ), spawn $ "variety -p && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_t ), spawn $ "variety -t && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_u ), spawn $ "wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")

  --CONTROL + SHIFT KEYS

  , ((controlMask .|. shiftMask , xK_Escape ), spawn $ "xfce4-taskmanager")

  -- Toggle microphone
  , ((mod1Mask .|. shiftMask, xK_m), spawn $ "amixer -q set Capture toggle")

  --SCREENSHOTS

  , ((0, xK_Print), spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask, xK_Print), spawn $ "flameshot gui" )
  , ((controlMask .|. shiftMask, xK_Print), spawn $ "flameshot screen -c" )
  , ((mod1Mask, xK_Print), spawn $ "flameshot screen -p ~/SS/ ")

  -- Scratchpads
  , ((0, xK_F12), namedScratchpadAction myScratchPads "scratchterminal")
  , ((0, xK_F11), namedScratchpadAction myScratchPads "scratchhtop")
  , ((0, xK_F10), namedScratchpadAction myScratchPads "floatingSpotify")

  --MULTIMEDIA KEYS

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn $ "pulse-volume.sh toggle")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -q set Master 5%-")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn $ "amixer -q set Master 5%+")

  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 5")

  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 5")

--  , ((0, xF86XK_AudioPlay), spawn $ "mpc toggle")
--  , ((0, xF86XK_AudioNext), spawn $ "mpc next")
--  , ((0, xF86XK_AudioPrev), spawn $ "mpc prev")
--  , ((0, xF86XK_AudioStop), spawn $ "mpc stop")


  , ((0, xF86XK_AudioPlay), spawn $ "playerctl play-pause")

  -- spotify pause / play
  , ((0 .|. shiftMask, xF86XK_AudioPlay), spawn $ "playerctl -p spotify play-pause")
  , ((0, xF86XK_AudioNext), spawn $ "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn $ "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn $ "playerctl stop")


  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  --Focus selected window
  , ((mod1Mask, xK_Tab), windows W.focusDown)

  --Focus previous window
  , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Left ), prevWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Right ), nextWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the master window.
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused windows with the master window
  , ((modMask .|. shiftMask, xK_o), promote)

  -- Allow windows to be over the xmobar
  , ((controlMask .|. modMask, xK_Down), sendMessage ToggleStruts)

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Swap the focused window with the previous window.
  , ((controlMask .|. modMask, xK_Up), windows W.swapUp  )

  -- Shrink the master area.
  , ((controlMask .|. shiftMask , xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((controlMask .|. shiftMask , xK_l), sendMessage Expand)
  , ((controlMask .|. shiftMask, xK_i), sendMessage $ MirrorExpand)               -- increment the right-hand gap

  -- Push window back into tiling.
  , ((controlMask .|. shiftMask , xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))

  -- Workspaces
  , ((mod1Mask .|. controlMask, xK_Tab), nextScreen)

  -- Hide window
  , ((modMask,               xK_m     ), withFocused minimizeWindow >>  windows W.focusUp)
  -- Show window
  , ((modMask .|. shiftMask, xK_m     ), withLastMinimized maximizeWindowAndFocus)
  ]
  ++

  -- ctrl-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- ctrl-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_1, xK_2] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  -- Super + [1..9], switch to workspace N
  -- Super + shift + [1..9], move client to workspace N and follow
  -- Super + control + [1..9], move client to workspace N
  -- 
  ++
  [((m .|. modMask, k), windows $ f i)
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]
      , (f, m) <- [ (W.greedyView                    , 0)
                  , (W.shift                         , shiftMask)
                  , (\i -> W.greedyView i . W.shift i, shiftMask)
                  , (W.shift                         , controlMask) ]]
      where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
            nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))




main :: IO ()
main = do
    xmobar0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobars/.xmobarrc"
    xmobar1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobars/.xmobarrc1"

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad $ ewmh def
        {
 layoutHook         = showWName' myShowWNameTheme myLayoutHook
, startupHook = myStartupHook
, manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> namedScratchpadManageHook myScratchPads <+> manageDocks
, handleEventHook    = serverModeEventHookCmd
           <+> serverModeEventHook
           <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
           <+> docksEventHook
, modMask = myModMask
, terminal = myTerminal
, borderWidth = myBorderWidth
, focusedBorderColor = focdBord
, focusFollowsMouse = myFocusFollowsMouse
, normalBorderColor = normBord
, keys = myKeys
, mouseBindings = myMouseBindings
, workspaces = myWorkspaces
, logHook = workspaceHistoryHook <+> dynamicLogWithPP xmobarPP
  { ppOutput = \x -> hPutStrLn xmobar0 x  -- >> hPutStrLn xmobar1 x
  , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
  , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
  , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""        -- Hidden workspaces in  xmobar
  , ppHiddenNoWindows= xmobarColor "#7a6a61" ""       -- Only shows visible workspaces. Useful for TreeSelect.
  , ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
  , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
  , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
  , ppExtras  = [windowCount]                           -- # of windows current workspace
  , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
  }
}
