{-# OPTIONS_GHC -W #-}
module Main where

import Data.Functor ((<$>))
import System.IO

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WorkspaceScreenshot (initCapturing)

import           Bindings
import           Profile
import qualified Startup
import           Themes
import           Workspaces

main :: IO ()
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
    , keys               = myKeyboardBindings
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

myLogHook :: Handle -> X ()
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

myLayoutHook = smartBorders $ avoidStruts $
  onWorkspace Texts (tabbed shrinkText myTheme) $
  onWorkspaces [Video, Mail, Files, Torrents] Full $
  onWorkspace Status (dwmStyle shrinkText myTheme tall) $
  onWorkspace Talkative (TwoPane (3/100) (1/2)) $
  tall ||| Mirror tall |||  Full
 where
  tall = Tall 1 0.03 0.5
