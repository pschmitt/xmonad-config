import XMonad
import qualified Data.Map as M
import Data.Monoid
import System.Exit
-- import XMonad hiding ( (|||) )
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS -- Switch to left|right WS, toggle WS etc
import XMonad.Actions.DynamicWorkspaces -- Add/delete WS on the fly
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.FindEmptyWorkspace -- Switch to an empty WS
import qualified XMonad.Actions.FlexibleResize as Flex -- Resize windows with mouse from any corner
import XMonad.Actions.GridSelect
--import XMonad.Actions.PhysicalScreens
--import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook -- Notifications
import XMonad.Layout.BoringWindows
import XMonad.Layout.Combo
import XMonad.Layout.Gaps
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutCombinators
--import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Named -- Rename Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Shell -- Xmonad launcher
import qualified XMonad.StackSet as W
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Paste
import XMonad.Util.Run
-- import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

myTerminal = "urxvtc"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False -- Sloppy focus
myBorderWidth = 0
defaultModMask = mod4Mask
statusBarHeight = "16"

--
-- Fonts
--
myFont       = "-*-tamsyn-*-*-*-*-12-*-*-*-*-*-*-*"
myUrgentFont = "-*-monospace-*-*-*-*-14-*-*-*-*-*-*-*"

--
-- Colors
--
myNormalBorderColor  = colorGray
myFocusedBorderColor = colorMagenta
colorBlack       = "#000000"
colorBlackAlt    = "#040404"
colorGray        = "#606060"
colorGrayAlt     = "#282828"
colorDarkGray    = "#161616"
colorWhite       = "#cfbfad"
colorWhiteAlt    = "#8c8b8e"
colorDarkWhite   = "#444444"
colorCream       = "#a9a6af"
colorDarkCream   = "#5f656b"
colorMagenta     = "#a488d9"
colorMagentaAlt  = "#7965ac"
colorDarkMagenta = "#8e82a2"
colorBlue        = "#98a7b6"
colorBlueAlt     = "#598691"
colorDarkBlue    = "#464a4a"
colorRed         = "#FF0000"
colorRedAlt      = "#A90000"
colorGreen       = "#40e740"
colorSeparator   = "#303030"

myXPConfig = defaultXPConfig {
    position = Top,
    height   = 16,
    font     = myFont,
    fgColor  = colorWhiteAlt,
    bgColor  = colorDarkGray,
    bgHLight = colorDarkGray,
    fgHLight = colorRed,
    promptBorderWidth = myBorderWidth
}

--
-- Workspaces
--
mainWs = "#!"
webWs  = "net"
tvWs   = "tv"
vidWs  = "vid"
chrmWs = "y2t"
devWs  = "dev"
workWs = "wrk"
dlWs   = "dl"
rdWs   = "rd"
myWorkspaces = [mainWs, webWs, tvWs, vidWs, chrmWs, devWs, workWs, dlWs, rdWs]

--
-- Layouts
--
myLayout = gaps [(XMonad.Layout.Gaps.R, 0)]
         -- $ Mag.magnifier
         $ avoidStruts
         $ smartBorders
         $ maximize
         $ minimize
         $ toggleLayouts Full
         $ boringAuto (tiled
                       XMonad.Layout.LayoutCombinators.||| Mirror tiled
                       XMonad.Layout.LayoutCombinators.||| combo
                       XMonad.Layout.LayoutCombinators.||| mouseResizableTile
                       XMonad.Layout.LayoutCombinators.||| simpleTabbed
                       XMonad.Layout.LayoutCombinators.||| Full
                       XMonad.Layout.LayoutCombinators.||| grid
                       XMonad.Layout.LayoutCombinators.||| simplestFloat)
           where
               tiled   = ResizableTall nmaster delta ratio []
               nmaster = 1
               ratio   = 1/2
               delta   = 3/100
               combo   = combineTwo (ResizableTall nmaster delta ratio []) (simpleTabbed) (simpleTabbed)
               grid    = Grid (16/9)

--
-- Spawn commands
--
dzenBar1 = "dzen2 -xs 1 -dock -title-name 'xmonad_lbar' -u -x '0' -y '0' -h '" ++ statusBarHeight ++ "' -w '430' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorWhiteAlt  ++ "' -fn '" ++ myFont  ++ "' -e 'button3=;onstart=lower'"
dzenBar2 = "dzen2 -xs 2 -dock -title-name 'xmonad_lbar' -u -x '0' -y '0' -h '" ++ statusBarHeight ++ "' -w '430' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorWhiteAlt  ++ "' -fn '" ++ myFont  ++ "' -e 'button3=;onstart=lower'"
dzenBar3 = "dzen2 -xs 3 -dock -title-name 'xmonad_lbar' -u -x '0' -y '0' -h '" ++ statusBarHeight ++ "' -w '430' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorWhiteAlt  ++ "' -fn '" ++ myFont  ++ "' -e 'button3=;onstart=lower'"
dzenStatusBar = "zsh -c \"tee >(" ++ dzenBar1  ++ ") >(" ++ dzenBar2  ++ ") >(" ++ dzenBar3 ++ ")\""
dzens c = if c == 3 then "zsh -c \"tee >(" ++ dzenBar1  ++ ") >(" ++ dzenBar2  ++ ") >(" ++ dzenBar3 ++ ")\"" else if c == 2 then "zsh -c \"tee >(" ++ dzenBar1  ++ ") >(" ++ dzenBar2  ++ ")\"" else dzenBar1
-- TODO: dzenStatusBar = dzens screenCount

-- TODO
-- if screenCount == 1 then
--     dzenStatusBar = dzen1Bar
-- else if screenCount == 2 then
--     dzenStatusBar = "zsh -c \"tee >(" ++ dzenBar1  ++ ") >(" ++ dzenBar2  ++ ")\""
-- else if screenCount == 3 then
--     dzenStatusBar = "zsh -c \"tee >(" ++ dzenBar1  ++ ") >(" ++ dzenBar2  ++ ") >(" ++ dzenBar3 ++ ")\""

restartCmd = "killall status.sh dzen2 conky; xmonad --recompile; xmonad --restart"
--restartCmd = "xkill -display :0 -id $(xwininfo -name 'xmonad_rbar' | grep 'Window id' | awk '{ print $4 }'); xkill -display :0-id $(xwininfo -name 'xmonad_lbar' | grep 'Window id' | awk '{ print $4 }'); xmonad --recompile; xmonad --restart;"
-- testCmd = ""

-- UrgencyHook
-- https://github.com/vdemeester/xmonad-config/blob/master/.xmonad/xmonad.hs
-- We are going to use notify-send
data NotifyUrgencyHook = NotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook NotifyUrgencyHook where
    urgencyHook NotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                  spawn $ "notify-send " ++ "\"Urgent Window\" \"<b>" ++ (show name ++ "</b> requests your attention on workspace <b>" ++ index) ++ "</b>\""

main = do
    d <- spawnPipe dzenStatusBar
    xmonad $ withUrgencyHookC NotifyUrgencyHook urgencyConfig { suppressWhen = Visible }
           $ ewmh
           $ defaultConfig {
                 -- simple stuff
                 terminal = myTerminal,
                 focusFollowsMouse = myFocusFollowsMouse,
                 borderWidth = myBorderWidth,
                 modMask = defaultModMask,
                 -- numlockMask deprecated in 0.9.1
                 -- numlockMask = myNumlockMask,
                 workspaces = myWorkspaces,
                 normalBorderColor = myNormalBorderColor,
                 focusedBorderColor = myFocusedBorderColor,
                 -- key bindings
                 keys = myKeys,
                 mouseBindings = myMouseBindings,
                 -- hooks, layouts
                 layoutHook = myLayout,
                 manageHook = myManageHook,
                 handleEventHook = fullscreenEventHook, -- fixes chrome fullscreen 
                 logHook = myLogHook d, -- >> updatePointer (Relative 0.5 0.5),
                 startupHook = myStartupHook
             }

--
-- Keybindings
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ -- Scratchpads
      ((modm .|. shiftMask, xK_n), namedScratchpadAction myScratchPads "music"),
      ((modm .|. shiftMask, xK_p), namedScratchpadAction myScratchPads "pulse"),
      ((modm .|. shiftMask, xK_t), namedScratchpadAction myScratchPads "terminal"),
      ((modm .|. shiftMask, xK_v), namedScratchpadAction myScratchPads "keyboard"),
      ((modm, xK_e), namedScratchpadAction myScratchPads "filebrow"),
      -- Launcher
      ((modm, xK_p), shellPrompt myXPConfig),
      ((modm, xK_r), shellPrompt myXPConfig),
      -- Kill client
      ((modm, xK_c), kill),
      ((modm  .|. shiftMask, xK_c), kill),
      -- Pasting
      ((shiftMask, xK_Insert), pasteSelection),
      -- Layout manipulation
      ((modm, xK_space ), sendMessage NextLayout),
      ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf),
      ((modm, xK_n), refresh),
      ((modm, xK_Tab), windows W.focusDown),
      ((modm, xK_j), windows W.focusDown),
      ((modm, xK_k), windows W.focusUp),
      ((modm, xK_m), windows W.focusMaster),
      ((modm, xK_Return), windows W.swapMaster),
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      ((modm, xK_h), sendMessage Shrink),
      ((modm, xK_l), sendMessage Expand),
      ((modm, xK_t), withFocused $ windows . W.sink),
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      ((modm, xK_b), sendMessage ToggleStruts),
      ((modm .|. controlMask, xK_t), sendMessage $ ToggleGaps),
      ((modm .|. controlMask, xK_a), sendMessage $ IncGap 20 XMonad.Layout.Gaps.R),  -- increment the right-hand gap
      ((modm .|. controlMask, xK_s), sendMessage $ DecGap 20 XMonad.Layout.Gaps.R),  -- decrement the right-hand gap
      ((modm, xK_g), goToSelected defaultGSConfig),
      -- Magnifier
      --((modm .|. controlMask              , xK_plus ), sendMessage Mag.MagnifyMore),
      --((modm .|. controlMask              , xK_minus), sendMessage Mag.MagnifyLess),
      --((modm .|. controlMask              , xK_o), sendMessage Mag.ToggleOff),
      --((modm .|. controlMask .|. shiftMask, xK_o), sendMessage Mag.ToggleOn),
      --((modm .|. controlMask              , xK_m), sendMessage Mag.Toggle),
      -- Exit / Restart
      ((modm .|. shiftMask, xK_Escape), io (exitWith ExitSuccess)),
      ((modm .|. shiftMask, xK_r), spawn restartCmd),
      ((modm .|. mod1Mask, xK_r), spawn restartCmd),
      -- Dynamic WS
      ((modm .|. shiftMask, xK_BackSpace), removeWorkspace),
      ((modm .|. shiftMask, xK_a), addWorkspacePrompt myXPConfig),
      ((modm .|. shiftMask, xK_b), DO.swapWith Next AnyWS),
      ((modm .|. shiftMask, xK_g), DO.swapWith Prev AnyWS),
      ((modm .|. shiftMask, xK_F2), renameWorkspace myXPConfig),
      -- Sticky
      ((modm, xK_s ), windows copyToAll), -- Make focused window sticky
      ((modm .|. shiftMask, xK_s), killAllOtherCopies), -- Unstick window
      -- Maximize / Fullscreen
      ((modm .|. shiftMask, xK_m), withFocused (sendMessage . maximizeRestore)),
      ((modm, xK_f), sendMessage (Toggle "Full")), -- Fullscreen w/o hiding dock
      ((modm .|. shiftMask, xK_f), do -- "real" fullscreen (hides dock)
                                        sendMessage (Toggle "Full")
                                        sendMessage ToggleStruts),
      -- Minimize
      ((modm, xK_u), withFocused minimizeWindow),
      ((modm .|. shiftMask, xK_u), sendMessage RestoreNextMinimizedWin)
      -- Test
      -- ((modm, xK_y), spawn testCmd)
    ]

    ++
    [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    -- zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
    -- ++
    -- zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- ++
    -- [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_F1, xK_F2, xK_F3] [0..],
    --          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    -- ]

    ++
    [ ((modm, xK_Left), DO.moveTo Prev (WSIs notSP)),
      ((modm, xK_Right), DO.moveTo Next (WSIs notSP)),
      -- Focus next/previous screen
      ((modm .|. mod1Mask, xK_Left), nextScreen),
      ((modm .|. mod1Mask, xK_Right), prevScreen), 
      ((modm, xK_Up), toggleWS),
      ((modm, xK_Down), moveTo Next EmptyWS)]
        where notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
        -- | any workspace but scratchpad
        --shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        -- >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
        -- | hidden, non-empty workspaces less scratchpad
        --shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
        -- >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
        --getSortByIndexNoSP = fmap (.scratchpadFilterOutWorkspace) getSortByIndex
        -- | toggle any workspace but scratchpad
        --myToggle = windows $ W.view =<< W.tag . head . filter
        -- ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden

--
-- Ḿouse bindings
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)),
      ((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
      ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster)) ]

myManageHook = composeOne
    [ 
      isDialog               -?> doFloat,
      className =? "MPlayer" -?> doFloat,
      className =? "Gimp" -?> doFloat,
      className =? "Zenity" -?> doFloat,
      className =? "XVkbd"   -?> doIgnore,
      --className =? "Keyboard" -?> doIgnore,
      --title     =? "Keyboard"      -?> doIgnore,
      title     =? "YouTube TV - Mozilla Firefox" -?> doFullFloat,
      className =? "YouTube TV - Mozilla Firefox" -?> doFullFloat,
      resource  =? "desktop_window" -?> doIgnore,
      resource  =? "kdesktop" -?> doIgnore,
     (className =? "Firefox" <&&> resource =? "Navigator") -?> doF (W.shift webWs) <+> unfloat,
      appName   =? "tmux" -?> doF (W.shift mainWs),
      className =? "Chromium" -?> doF (W.shift webWs),
      className =? "luakit" -?> doF (W.shift webWs),
      className =? "uzbl-tabbed" -?> doF (W.shift webWs),
      className =? "Eclipse" -?> doF (W.shift devWs),
      className =? "jetbrains-idea-ce" -?> doF (W.shift devWs),
      className =? "jetbrains-android-studio" -?> doF (W.shift devWs),
      className =? "jd-Main" -?> doF (W.shift dlWs),
      className =? "Vlc" -?> doF (W.shift vidWs),
      className =? "xbmc.bin" -?> doF (W.shift tvWs) <+> doFullFloat,
      className =? "rdesktop" -?> doF (W.shift rdWs),
      className =? "stalonetray" -?> doIgnore,
      --isFullscreen -?> (doF W.focusDown <+> doFullFloat)
      isFullscreen -?> doFullFloat
    ] <+> namedScratchpadManageHook myScratchPads <+> manageDocks -- <+> doFloat
    where unfloat = ask >>= doF . W.sink

--
-- Scratchpads
--
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm,
                  NS "keyboard" spawnKb findKb manageTerm,
                  NS "music" spawnMusic findMusic manageMusic,
                  NS "pulse" spawnPulse findPulse managePulse,
                  NS "filebrow" spawnFiles findFiles manageFiles ]

  where
    spawnMusic  = myTerminal ++ " -name ncmpcpp -e ncmpcpp" -- launch term + ncmpcpp
    findMusic   = resource =? "ncmpcpp" -- its window will be named "ncmpcpp" (see above)
    manageMusic = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below:
      where
        h = 0.6 -- height, 60%
        w = 0.6 -- width, 60
        t = (1 - h)/2 -- centered top/bottom
        l = (1 - w)/2 -- centered left/right

    spawnPulse  = "pavucontrol" 
    findPulse   = className =? "Pavucontrol" -- its window will be named "ncmpcpp" (see above)
    managePulse = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below:
      where
        h = 0.8 -- height, 60%
        w = 0.8 -- width, 60
        t = (1 - h)/2 -- centered top/bottom
        l = (1 - w)/2 -- centered left/right

    spawnTerm  = myTerminal ++ " -name scratchpad" -- launch my terminal
    findTerm   = resource =? "scratchpad" -- its window will be named "scratchpad" (see above)
    manageTerm = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        h = 0.2 -- height, 20%
        w = 1 -- width, 100%
        t = 1 - h -- bottom edge
        l = (1 - w)/2 -- centered left/right

    spawnKb  = "matchbox-keyboard"
    findKb   = title =? "Keyboard" -- its window will be named "keyboard" (see above)
    manageKb = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        h = 0.25 -- height, 25%
        w = 1 -- width, 100%
        t = 1 - h -- bottom edge
        l = (1 - w)/2 -- centered left/right

    spawnFiles  = "spacefm"
    findFiles   = resource =? "spacefm" -- <&&> title /=? "Open Location" <&&> title /=? "File Manager Preferences"
    manageFiles = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        h = 0.8 -- height, 60%
        w = 0.6 -- width, 60%
        t = (1 - h)/2 -- centered top/bottom
        l = (1 - w)/2 -- centered left/right

dzenSwitchWs :: String -> String
dzenSwitchWs s = "^ca(1,~/bin/switch-workspace.sh " ++ (show s) ++ ")" ++ s ++ "^ca()"

myLogHook h = dynamicLogWithPP $ defaultPP
       {  ppSort    = fmap (namedScratchpadFilterOutWorkspace.) (DO.getSortByOrder),
          ppCurrent = dzenColor colorBlueAlt colorDarkGray . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId,
          ppVisible = dzenColor colorCream colorDarkGray . dzenSwitchWs,
          ppHidden  = dzenColor "" colorDarkGray . dzenSwitchWs,
          ppUrgent  = dzenColor colorGreen "" . dzenSwitchWs,
          ppWsSep   = " ",
          ppSep     = "^fg(" ++ colorSeparator ++ ") | ^fg()",
          ppTitle   = dzenColor colorWhiteAlt "" . wrap "< " " >",
          ppOutput  = hPutStrLn h,
          -- Hide empty ws
          --ppHiddenNoWindows = dzenColor colorDarkWhite colorDarkGray . dzenSwitchWs,
          ppLayout  = dzenColor colorBlueAlt colorDarkGray .
               (\x -> case x of
                    "Maximize Minimize ResizableTall"        -> "[]="
                    "Maximize Minimize Mirror ResizableTall" -> "=[]"
                    "Maximize Minimize MouseResizableTile"   -> "[m]"
                    "Maximize Minimize SimplestFloat"        -> "[-]"
                    "Maximize Minimize Tall"                 -> "[t]"
                    "Maximize Minimize Mirror Tall"          -> "[T]"
                    "Maximize Minimize Tabbed Simplest"      -> "[=]"
                    "Maximize Minimize Full"                 -> "[F]"
                    "Maximize Minimize Grid"                 -> "[g]"
                    "Maximize Minimize combining Tabbed Simplest and Tabbed Simplest with ResizableTall" -> "[*]")
       }


myStartupHook = startup
startup :: X ()
startup = do
    spawn "~/.config/xmonad/status.sh"
    setWMName "LG3D"
