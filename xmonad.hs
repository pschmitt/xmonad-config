import XMonad
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS -- Switch to left|right WS, toggle WS etc
import XMonad.Actions.DynamicWorkspaces -- Add/delete WS on the fly
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.FindEmptyWorkspace -- Switch to an empty WS
import qualified XMonad.Actions.FlexibleResize as Flex -- Resize windows with mouse from any corner
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
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
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.LayoutCombinators
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
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare

terminal = "urxvtc"
focusFollowsMouse :: Bool
focusFollowsMouse = True -- True: Sloppy focus
clickJustFocuses :: Bool
clickJustFocuses = True
borderWidth = 0
defaultModMask = mod4Mask
statusBarHeight = "20"
statusBarWidth  = "430"

--
-- Fonts
--
font       = "-*-tamsyn-*-*-*-*-16-*-*-*-*-*-*-*"
-- urgentFont = "-*-monospace-*-*-*-*-14-*-*-*-*-*-*-*"

--
-- Colors
--
normalBorderColor  = colorGray
focusedBorderColor = colorMagenta
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

xpConfig = defaultXPConfig {
      position = Top
    , height   = 20 --statusBarHeight
    , XMonad.Prompt.font = Main.font
    , fgColor  = colorWhiteAlt
    , bgColor  = colorDarkGray
    , bgHLight = colorDarkGray
    , fgHLight = colorRed
    , promptBorderWidth = Main.borderWidth
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
workspaces = [mainWs, webWs, tvWs, vidWs, chrmWs, devWs, workWs, dlWs, rdWs]

--
-- Layouts
--
layout = gaps [(XMonad.Layout.Gaps.R, 0), (XMonad.Layout.Gaps.D, 0)]
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
dzenBar1 = "dzen2 -xs 1 -dock -title-name 'xmonad_lbar' -u -x '0' -y '0' -h '" ++ statusBarHeight
        ++ "' -w '" ++ statusBarWidth ++ "' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorWhiteAlt
        ++ "' -fn '" ++ Main.font  ++ "' -e 'button3=;onstart=lower'"
dzenBar2 = "dzen2 -xs 2 -dock -title-name 'xmonad_lbar' -u -x '0' -y '0' -h '" ++ statusBarHeight
        ++ "' -w '" ++ statusBarWidth ++ "' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorWhiteAlt
        ++ "' -fn '" ++ Main.font  ++ "' -e 'button3=;onstart=lower'"
dzenBar3 = "dzen2 -xs 3 -dock -title-name 'xmonad_lbar' -u -x '0' -y '0' -h '" ++ statusBarHeight
        ++ "' -w '" ++ statusBarWidth ++ "' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorWhiteAlt
        ++ "' -fn '" ++ Main.font  ++ "' -e 'button3=;onstart=lower'"

dzens c =
        if c == 3 then
            "zsh -c \"tee >(" ++ dzenBar1  ++ ") >(" ++ dzenBar2  ++ ") >(" ++ dzenBar3 ++ ")\""
        else if c == 2 then
            "zsh -c \"tee >(" ++ dzenBar1  ++ ") >(" ++ dzenBar2  ++ ")\""
        else
            dzenBar1

dzenStatusBar = dzens 1

widgetCmd = "~/.xmonad/status.sh -x '" ++ statusBarWidth ++ "' -h '" ++ statusBarHeight
          ++ "' --fn '" ++ Main.font ++ "' --bg '" ++ colorDarkGray ++ "' --fg '" ++ colorWhiteAlt ++ "'"

restartCmd = "xmonad --recompile && { killall status.sh dzen2 conky; xmonad --restart; }"

-- UrgencyHook
-- https://github.com/vdemeester/xmonad-config/blob/master/.xmonad/xmonad.hs
-- We are going to use notify-send
data NotifyUrgencyHook = NotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook NotifyUrgencyHook where
    urgencyHook NotifyUrgencyHook w = do
        name <- getName w
        ws   <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                  spawn $ "notify-send " ++ "\"Urgent Window\" \"<b>" ++ (show name ++ "</b> requests your attention on workspace <b>" ++ index) ++ "</b>\""

main = do
    nScreens <- countScreens
    d <- spawnPipe dzenStatusBar
    xmonad $ withUrgencyHookC NotifyUrgencyHook urgencyConfig { suppressWhen = Visible }
           $ ewmh
           $ defaultConfig {
                 -- simple stuff
                   XMonad.terminal           = Main.terminal
                 , XMonad.focusFollowsMouse  = Main.focusFollowsMouse
                 , XMonad.clickJustFocuses   = Main.clickJustFocuses
                 , XMonad.borderWidth        = Main.borderWidth
                 , XMonad.modMask            = Main.defaultModMask
                 , XMonad.workspaces         = Main.workspaces
                 , XMonad.normalBorderColor  = Main.normalBorderColor
                 , XMonad.focusedBorderColor = Main.focusedBorderColor
                 -- key bindings
                 , XMonad.keys               = Main.keys
                 , XMonad.mouseBindings      = Main.mouseBindings
                 -- hooks, layouts
                 , XMonad.layoutHook         = Main.layout
                 , XMonad.manageHook         = Main.manageHook
                 , XMonad.handleEventHook    = fullscreenEventHook -- fixes chrome fullscreen
                 , XMonad.logHook            = Main.logHook d -- >> updatePointer (Relative 0.5 0.5)
                 , XMonad.startupHook        = Main.startupHook
           }

--
-- Keybindings
--
keys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ -- Scratchpads
        ((modm .|. shiftMask, xK_n), namedScratchpadAction scratchpads "music")
      , ((modm .|. shiftMask, xK_p), namedScratchpadAction scratchpads "pulse")
      , ((modm .|. shiftMask, xK_t), namedScratchpadAction scratchpads "terminal")
      , ((modm .|. shiftMask, xK_v), do namedScratchpadAction scratchpads "keyboard"
                                        sendMessage $ IncGap 270 XMonad.Layout.Gaps.D)
      , ((modm, xK_e), namedScratchpadAction scratchpads "filebrow")
      -- Launcher
      , ((modm, xK_p), shellPrompt xpConfig)
      , ((modm, xK_r), shellPrompt xpConfig)
      -- Kill client
      , ((modm, xK_c), kill)
      , ((modm  .|. shiftMask, xK_c), kill)
      -- Pasting
      , ((shiftMask, xK_Insert), pasteSelection)
      -- Layout manipulation
      , ((modm, xK_space ), sendMessage NextLayout)
      , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
      , ((modm, xK_n), refresh)
      , ((modm, xK_Tab), windows W.focusDown)
      , ((modm, xK_j), windows W.focusDown)
      , ((modm, xK_k), windows W.focusUp)
      , ((modm, xK_m), windows W.focusMaster)
      , ((modm, xK_Return), windows W.swapMaster)
      , ((modm .|. shiftMask, xK_j), windows W.swapDown)
      , ((modm .|. shiftMask, xK_k), windows W.swapUp)
      , ((modm, xK_h), sendMessage Shrink)
      , ((modm, xK_l), sendMessage Expand)
      , ((modm, xK_t), withFocused $ windows . W.sink)
      , ((modm, xK_comma), sendMessage (IncMasterN 1))
      , ((modm, xK_period), sendMessage (IncMasterN (-1)))
      , ((modm, xK_b), sendMessage ToggleStruts)
      -- Gaps
      , ((modm .|. controlMask, xK_t), sendMessage $ ToggleGaps)
      , ((modm .|. controlMask, xK_a), sendMessage $ IncGap 20 XMonad.Layout.Gaps.R)  -- increment the right-hand gap
      , ((modm .|. controlMask, xK_s), sendMessage $ DecGap 20 XMonad.Layout.Gaps.R)  -- decrement the right-hand gap
      -- Go to
      , ((modm, xK_g), goToSelected defaultGSConfig)
      -- Exit / Restart
      , ((modm .|. shiftMask, xK_Escape), io (exitWith ExitSuccess))
      , ((modm .|. shiftMask, xK_r), spawn restartCmd)
      , ((modm .|. mod1Mask, xK_r), spawn restartCmd)
      -- Dynamic WS
      , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
      , ((modm .|. shiftMask, xK_a), addWorkspacePrompt xpConfig)
      , ((modm .|. shiftMask, xK_b), DO.swapWith Next AnyWS)
      , ((modm .|. shiftMask, xK_g), DO.swapWith Prev AnyWS)
      , ((modm .|. shiftMask, xK_F2), renameWorkspace xpConfig)
      -- Sticky
      , ((modm, xK_s ), windows copyToAll) -- Make focused window sticky
      , ((modm .|. shiftMask, xK_s), killAllOtherCopies) -- Unstick window
      -- Maximize / Fullscreen
      , ((modm .|. shiftMask, xK_m), withFocused (sendMessage . maximizeRestore))
      , ((modm, xK_f), sendMessage (Toggle "Full")) -- Fullscreen w/o hiding dock
      , ((modm .|. shiftMask, xK_f), do -- "real" fullscreen (hides dock)
                                        sendMessage (Toggle "Full")
                                        sendMessage ToggleStruts)
      -- Minimize
      , ((modm, xK_u), withFocused minimizeWindow)
      , ((modm .|. shiftMask, xK_u), sendMessage RestoreNextMinimizedWin)
    ]

    ++
    [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

    ++
      [ ((modm, xK_Left), DO.moveTo Prev (WSIs notSP))
      , ((modm, xK_Right), DO.moveTo Next (WSIs notSP))
      -- Focus next/previous screen
      , ((modm .|. mod1Mask, xK_Left), nextScreen)
      , ((modm .|. mod1Mask, xK_Right), prevScreen)
      , ((modm, xK_Up), toggleWS)
      , ((modm, xK_Down), moveTo Next EmptyWS)
      ] where notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)

--
-- Ḿouse bindings
--
mouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
      [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
      , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
      , ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster)) ]

manageHook = composeOne
      [ isDialog                -?> doFloat
      , className =? "Zenity"   -?> doFloat
      , className =? "XVkbd"    -?> doIgnore
      , resource  =? "desktop_window" -?> doIgnore
      -- Browsers
      , (className =? "Firefox" <&&> resource =? "Navigator") -?> doF (W.shift webWs) <+> unfloat
      , title     =? "YouTube TV - Mozilla Firefox" -?> doFullFloat
      , className =? "YouTube TV - Mozilla Firefox" -?> doFullFloat
      , appName   =? "tmux"     -?> doF (W.shift mainWs)
      , className =? "Chromium" -?> doF (W.shift webWs)
      , className =? "luakit"   -?> doF (W.shift webWs)
      , className =? "uzbl-tabbed" -?> doF (W.shift webWs)
      -- IDE
      , className =? "Eclipse"                  -?> doF (W.shift devWs)
      , className =? "jetbrains-idea-ce"        -?> doF (W.shift devWs)
      , className =? "jetbrains-android-studio" -?> doF (W.shift devWs)
      -- Multimedia
      , className =? "MPlayer"     -?> doFloat
      , className =? "Gimp"        -?> doFloat
      , className =? "jd-Main"     -?> doF (W.shift dlWs)
      , className =? "Vlc"         -?> doF (W.shift vidWs)
      , className =? "xbmc.bin"    -?> doF (W.shift tvWs) <+> doFullFloat
      , className =? "stalonetray" -?> doIgnore
      , isFullscreen -?> doFullFloat
      ] <+> namedScratchpadManageHook scratchpads <+> manageDocks -- <+> doFloat
        where unfloat = ask >>= doF . W.sink

-- Scratchpads
--
scratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
              , NS "keyboard" spawnKb findKb manageTerm
              , NS "music" spawnMusic findMusic manageMusic
              , NS "pulse" spawnPulse findPulse managePulse
              , NS "filebrow" spawnFiles findFiles manageFiles ]
  where
    spawnMusic  = Main.terminal ++ " -name ncmpcpp -e ncmpcpp" -- launch term + ncmpcpp
    findMusic   = resource =? "ncmpcpp" -- its window will be named "ncmpcpp" (see above)
    manageMusic = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below:
      where
        h = 0.6 -- height, 60%
        w = 0.6 -- width, 60%
        t = (1 - h) / 2 -- centered top/bottom
        l = (1 - w) / 2 -- centered left/right

    spawnPulse  = "pavucontrol"
    findPulse   = className =? "Pavucontrol" -- its window will be named "Pavucontrol"
    managePulse = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below:
      where
        h = 0.8 -- height, 80%
        w = 0.8 -- width, 80%
        t = (1 - h) / 2 -- centered top/bottom
        l = (1 - w) / 2 -- centered left/right

    spawnTerm  = Main.terminal ++ " -name scratchpad" -- launch my terminal
    findTerm   = resource =? "scratchpad" -- its window will be named "scratchpad" (see above)
    manageTerm = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        h = 0.25 -- height, 25%
        w = 1 -- width, 100%
        t = 1 - h -- bottom edge
        l = (1 - w) / 2 -- centered left/right

    spawnKb  = "matchbox-keyboard"
    findKb   = title =? "Keyboard" -- its window will be named "keyboard" (see above)
    manageKb = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        h = 0.25 -- height, 25%
        w = 1 -- width, 100%
        t = 1 - h -- bottom edge
        l = (1 - w) / 2 -- centered left/right

    spawnFiles  = "spacefm"
    findFiles   = resource =? "spacefm" -- <&&> title /=? "Open Location" <&&> title /=? "File Manager Preferences"
    manageFiles = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        h = 0.8 -- height, 80%
        w = 0.6 -- width, 60%
        t = (1 - h) / 2 -- centered top/bottom
        l = (1 - w) / 2 -- centered left/right

dzenSwitchWs :: String -> String
dzenSwitchWs s = "^ca(1,~/bin/switch-workspace.sh " ++ (show s) ++ ")" ++ s ++ "^ca()"

logHook h = dynamicLogWithPP $ defaultPP
       {    ppSort    = fmap (namedScratchpadFilterOutWorkspace.) (DO.getSortByOrder)
          , ppCurrent = dzenColor colorBlueAlt colorDarkGray . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
          , ppVisible = dzenColor colorCream colorDarkGray . dzenSwitchWs
          , ppHidden  = dzenColor "" colorDarkGray . dzenSwitchWs
          , ppUrgent  = dzenColor colorGreen "" . dzenSwitchWs
          , ppWsSep   = " "
          , ppSep     = "^fg(" ++ colorSeparator ++ ") | ^fg()"
          , ppTitle   = dzenColor colorWhiteAlt "" . wrap "< " " >"
          , ppOutput  = hPutStrLn h
          -- Hide empty ws
          --ppHiddenNoWindows = dzenColor colorDarkWhite colorDarkGray . dzenSwitchWs
          , ppLayout  = dzenColor colorBlueAlt colorDarkGray .
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

startupHook = startup
startup :: X ()
startup = do
    spawn widgetCmd
    setWMName "LG3D"
