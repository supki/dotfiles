import Data.Functor ((<$>))
import System.IO

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WorkspaceScreenshot (initCapturing)

import           Controls
import           Layouts
import           Profile
import qualified Startup
import           Workspaces

main = do
  initCapturing
  xmproc <- spawnPipe "xmobar"
  xmonad defaultConfig
    -- Terminal
    { terminal           = myTerminal
    -- Hooks
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook xmproc
    , startupHook        = Startup.myStartupHook
    -- Key bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    -- Other stuff
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    }

-- Log hook
myLogHook xmproc_handle = do
  updatePointer (Relative 0.5 0.5)
  dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmproc_handle
    , ppLayout = const ""
    , ppTitle = xmobarColor yellowColor ""
    , ppCurrent = xmobarColor yellowColor "" . const "λ"
    , ppHidden = xmobarColor orangeColor ""
    , ppHiddenNoWindows = xmobarColor grayLightColor ""
    , ppSep = "<fc=" ++ whiteColor ++ "> | </fc>"
    , ppSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort defaultPP
    }
--

