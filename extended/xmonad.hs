{-# LANGUAGE CPP #-}
module Main where

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.ManageDocks
import XMonad.Layout.DwmStyle
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
#ifdef HAS_SCREENSHOTER
import XMonad.Util.WorkspaceScreenshot (initCapturing)
#endif

import           Bindings
import           Profile
import qualified Startup
import           Themes
import           Workspaces

main :: IO ()
main = do
#ifdef HAS_SCREENSHOTER
  initCapturing
#endif
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
  onWorkspaces [Texts] (tabbedBottomAlways shrinkText myTheme) $
  onWorkspaces [Video, Mail, Files, Torrents] Full $
  threeCol ||| Full
 where
  threeCol = ThreeCol 1 (3/100) (1/3)
