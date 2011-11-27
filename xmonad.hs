import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.UpdatePointer
import XMonad.Layout.MouseResizableTile
import XMonad.StackSet as W (shift, greedyView)
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Layout.Tabbed
import System.IO


main = do
	xmproc <- spawnPipe "/usr/bin/xmobar -x 0 ~/.xmobarrc"
	xmonad $ addKeyBindings $ myConfig xmproc

myConfig xmproc = defaultConfig {
			manageHook = manageDocks <+> ( myManageHooks <+> manageHook defaultConfig ),
			layoutHook = smartBorders( avoidStruts (myLayout)),
			workspaces = myWorkspaces,
			logHook = myLoghook xmproc,
			modMask = mod4Mask,	 -- Rebind Mod to the Windows key
			startupHook = myStartuphook
		}  

addKeyBindings config = config `additionalKeys` (
			[ ((mod4Mask .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
			, ((mod4Mask, xK_p), spawn "dmenu_run")
			, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
			, ((mod4Mask .|. shiftMask, xK_F12), spawn "x-terminal-emulator -e 'gdm-control --shutdown && xmonad --restart'")
			, ((mod4Mask .|. shiftMask, xK_F11), spawn "x-terminal-emulator -e 'gdm-control --restart && xmonad --restart'")
			, ((mod4Mask .|. shiftMask, xK_Return), spawn "terminator")
			, ((mod4Mask .|. shiftMask, xK_r), renameWorkspace defaultXPConfig)
			--XF86AudioMute
			, ((0,  0x1008ff12), spawn "amixer sset Master toggle")
			-- XF86AudioLowerVolume
			, ((0 , 0x1008ff11), spawn "amixer -q sset Master 1- unmute")
			-- XF86AudioRaiseVolume
			, ((0 , 0x1008ff13), spawn "amixer -q set Master 1+ unmute")
			, ((mod4Mask, xK_u), sendMessage ShrinkSlave) -- %! Shrink a slave area
			, ((mod4Mask, xK_i), sendMessage ExpandSlave) -- %! Expand a slave area
			, ((0, xK_Print), spawn "scrot")
			]
			++
			[((m .|. mod4Mask, k), windows $ f i)                                                                           
				| (i, k) <- zip (drop 9 myWorkspaces) [xK_0, xK_minus, xK_equal]
				, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
		)
		`removeKeys`
		[ (mod4Mask, xK_q) ]

myStartuphook =  do
	setWMName "LG3D" --java hack
	setWorkspaceName (myWorkspaces!!0) "chrome"
	setWorkspaceName (myWorkspaces!!1) "vim"
	setWorkspaceName (myWorkspaces!!2) "personal"
	setWorkspaceName (myWorkspaces!!3) "git"
	setWorkspaceName (myWorkspaces!!11) "VM"
	spawnOn (myWorkspaces!!0) "google-chrome"
	spawnOn (myWorkspaces!!11) "truecrypt"
	spawnOn (myWorkspaces!!11) "virtualbox"

myLoghook xmproc = workspaceNamesPP defaultPP { 
					ppOutput = hPutStrLn xmproc,
					ppCurrent = xmobarColor "yellow" "" . wrap "[" "]",
					ppVisible = xmobarColor "#999900" "" . wrap "(" ")",
					ppUrgent = xmobarColor "blue" "gray",
					ppSep = " | ",
					ppLayout = xmobarColor "orange" "" . trim,
					ppTitle = xmobarColor "green" "" . trim
				} >>= dynamicLogWithPP >> takeTopFocus >> updatePointer (Relative 0.25 0.25)

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..12]

myManageHooks = composeAll . concat $
	[ [ isFullscreen --> doFullFloat ]
	, [(className =? "Firefox" <&&> appName =? "Dialog") --> doFloat] 
	, [(className =? "Iceweasel" <&&> appName =? "Dialog") --> doFloat] 
	, [(stringProperty "WM_WINDOW_ROLE" =? "Preferences") --> doFloat] 
	, [(title =? "volumeicon") --> doFloat]
	, [(title =? "Preferences") --> doFloat] 
	, [(className =? "Gimp") --> doFloat ] 
	, [(className =? "Truecrypt" <||> className =? "VirtualBox") --> doShift (myWorkspaces!!11) ] ]

myLayout = mouseResizableTile {
				masterFrac  = 0.6,
				draggerType = FixedDragger 0 3
			} 
			|||	mouseResizableTile {
				masterFrac = 0.6,
				draggerType = FixedDragger 0 3,
				isMirrored = True
			} ||| simpleTabbed
