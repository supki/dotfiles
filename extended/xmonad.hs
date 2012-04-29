import Data.Functor ((<$>))
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

import Controls
import Layouts
import Misc
import Startup
import Workspaces
import qualified Profile         as P

main = do
	xmproc <- spawnPipe "xmobar"
	xmonad $ defaultConfig
		-- Terminal
		{ terminal           = myTerminal
		-- Hooks
		, manageHook         = myManageHook
		, layoutHook         = myLayoutHook
		, logHook            = myLogHook xmproc
		, startupHook        = myStartupHook
		-- Key bindings
		, keys               = myKeys
		, mouseBindings      = myMouseBindings
		-- Other stuff
		, focusFollowsMouse  = myFocusFollowsMouse
		, modMask            = myModMask
		, workspaces         = myWorkspaces
		, borderWidth        = myBorderWidth
		, normalBorderColor  = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		}

-- Log hook
myLogHook xmproc_handle = dynamicLogWithPP $ xmobarPP
	{ ppOutput = hPutStrLn xmproc_handle
	, ppLayout = const ""
	, ppTitle = xmobarColor P.yellowColor ""
	, ppCurrent = xmobarColor P.yellowColor "" . const "λ"
	, ppHidden = xmobarColor P.orangeColor ""
	, ppHiddenNoWindows = xmobarColor P.grayLightColor ""
	, ppSep = "<fc=" ++ P.whiteColor ++ "> | </fc>"
    , ppSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort defaultPP
	}
--

