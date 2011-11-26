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
import System.IO


main = do
	xmproc <- spawnPipe "/usr/bin/xmobar -x 0 ~/.xmobarrc"
	xmonad $ addKeyBindings $ myConfig xmproc

--setWorkspaceName myWorkspaces!!0 "chrome"	

myConfig xmproc = defaultConfig {
			manageHook = manageDocks <+> ( myManageHooks <+> manageHook defaultConfig ),
			layoutHook = smartBorders( avoidStruts (myLayout)),
			workspaces = myWorkspaces,
			logHook = myLoghook xmproc,
			modMask = mod4Mask,	 -- Rebind Mod to the Windows key
			startupHook = setWMName "LG3D" --java hack
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
				| (i, k) <- zip (mySecondWorkspaces) [xK_0, xK_minus, xK_equal]
				, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
		)
		`removeKeys`
		[ (mod4Mask, xK_q) ]

myLoghook xmproc = workspaceNamesPP defaultPP { 
					ppOutput = hPutStrLn xmproc,
					ppCurrent = xmobarColor "yellow" "" . wrap "[" "]",
					ppVisible = xmobarColor "red" "" . wrap "(" ")",
					ppUrgent = xmobarColor "blue" "gray",
					ppSep = " | ",
					ppLayout = xmobarColor "orange" "" . trim,
					ppTitle = xmobarColor "green" "" . trim
				} >>= dynamicLogWithPP >> takeTopFocus >> updatePointer (Relative 0.25 0.25)

myWorkspaces :: [WorkspaceId]
myWorkspaces = myFirstWorkspaces ++ mySecondWorkspaces
myFirstWorkspaces = ["1","2:vim","3:personal","4:git","5","6","7","8","9"]
mySecondWorkspaces = ["10:VM","11","12:VM"]

myManageHooks = composeAll . concat $
	[ [ isFullscreen --> doFullFloat ]
	,  [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat] 
	,  [(title =? "volumeicon") --> doFloat]
	,  [(title =? "Preferences") --> doFloat] 
	,  [(className =? "Gimp") --> doFloat ] ]

myLayout = mouseResizableTile {
				masterFrac  = 0.6,
				draggerType = FixedDragger 0 3
			} 
			|||	mouseResizableTile {
				masterFrac = 0.6,
				draggerType = FixedDragger 0 3,
				isMirrored = True
			} ||| Full 
