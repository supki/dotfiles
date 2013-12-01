{-# OPTIONS_GHC -W #-}
module Main where

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.ManageDocks
import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Util.WorkspaceScreenshot (initCapturing)

import           Bindings
import           Profile
import qualified Startup
import           Themes
import           Workspaces

main :: IO ()
main = do
  initCapturing
  xmonad defaultConfig
    -- Terminal
    { terminal           = myTerminal
    -- Hooks
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook
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

myLogHook :: X ()
myLogHook = updatePointer (Relative 0.5 0.5)

myLayoutHook = smartBorders . avoidStruts $
  onWorkspace Texts (tabbed shrinkText myTheme) $
  onWorkspaces [Video, Mail, Files, Torrents] Full $
  onWorkspace Status (dwmStyle shrinkText myTheme tall) $
  onWorkspace Talkative (TwoPane (3/100) (1/2)) $
  tall ||| Mirror tall |||  Full
 where
  tall = Tall 1 0.03 0.5
