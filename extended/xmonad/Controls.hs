module Controls where

import Control.Monad
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import qualified Network.MPD as MPD
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.ManageDocks
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceScreenshot
import qualified XMonad.StackSet as W

import Themes
import qualified Profile as P
import qualified Workspaces as WS
import qualified Tmux as Tmux

-- Mouse
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
  -- swap the focused window and the master window
  [ ((modm, button2), \w -> focus w >> mouseMoveWindow w)
  -- resize window
  , ((modm, button3), \w -> focus w >> mouseResizeWindow w)
  -- copy modm+. and mod+, actions on mouse wheel
  , ((modm, button4), \w -> focus w >> sendMessage (IncMasterN 1))
  , ((modm, button5), \w -> focus w >> sendMessage (IncMasterN (-1)))
  ]
--


-- Keyboard hotkeys
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  -- default
  [ ((modm .|. mod, key), command)
    | (mod, key, command) <-
      -- launch a terminal
      [ (shiftMask,   xK_Return, spawn $ XMonad.terminal conf)
      -- launch tmuxPrompt
      , (0,           xK_Return, Tmux.prompt myXPConfig)
      -- launch shellPrompt
      , (0,           xK_p, shellPrompt myXPConfig)
      -- close focused window
      , (shiftMask,   xK_c, kill)
      -- rotate through the available layout algorithms
      , (0,           xK_space, sendMessage NextLayout)
      -- reset the layouts on the current workspace to default
      , (shiftMask,   xK_space, setLayout $ XMonad.layoutHook conf)
      -- resize viewed windows to the correct size
      , (0,           xK_n, refresh)
      -- toggle scratchpads
      , (0,           xK_s, namedScratchpadAction WS.scratchpads "scratchpad")
      , (0,           xK_o, namedScratchpadAction WS.scratchpads "vimus")
      -- switch to last workspace
      , (0,           xK_Tab, toggleWS' ["NSP"])
      -- move focus
      , (shiftMask,   xK_Tab, windows W.focusDown)
      , (0,           xK_j, windows W.focusDown)
      , (0,           xK_k, windows W.focusUp)
      -- swap the focused window
      , (controlMask, xK_Return, windows W.swapMaster)
      , (shiftMask,   xK_j, windows W.swapDown)
      , (shiftMask,   xK_k, windows W.swapUp)
      -- shrink/expand the master area
      , (0,           xK_h, sendMessage Shrink)
      , (0,           xK_l, sendMessage Expand)
      -- push window back into tiling
      , (0,           xK_t, withFocused $ windows . W.sink)
      -- increment/decrement the number of windows in the master area
      , (0,           xK_comma, sendMessage (IncMasterN 1))
      , (0,           xK_period, sendMessage (IncMasterN (-1)))
      -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
      , (0,           xK_b, sendMessage ToggleStruts)
      -- quit/restart xmonad
      , (0,           xK_q, restart "xmonad" True)
      , (shiftMask,   xK_q, spawn "killall trayer; xmonad --recompile; xmonad --restart")
      -- view empty workspace
      , (0,           xK_m, viewEmptyWorkspace)
      -- shift window to empty workspace
      , (shiftMask,   xK_m, tagToEmptyWorkspace)
      -- make workspaces screenshots and merge them
      , (shiftMask,   xK_u, captureWorkspacesWhenId (\x -> return $ x `notElem` ["4","5","-","\\"]) defaultHook horizontally)
      ]
      ++
      -- switch to workspace
      -- move client to workspace
      [ (m,           k, windows $ f i)
        | (i, k) <- zip WS.myWorkspaces WS.myWorkspacesKeys
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]
      ++
      -- switch to another screen
      [ (m,           k, screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]
      ++
      -- swap workspaces and return to old one
      [ (controlMask, k, withWindowSet (\s -> windows (swapWithCurrent i) >> windows (W.greedyView $ W.currentTag s)))
        | (i, k) <- zip WS.myWorkspaces WS.myWorkspacesKeys
      ]
  ]
  ++
  -- third party
  [ ((mod1Mask .|. mod, key), script)
    | (mod, key, script) <-
      [ (shiftMask, xK_comma,  io . void $ MPD.withMPD MPD.previous)
      , (shiftMask, xK_period, io . void $ MPD.withMPD MPD.next)
      , (shiftMask, xK_Return, io . void . MPD.withMPD $
          MPD.status >>= \s -> case MPD.stState s of
            MPD.Playing -> MPD.pause True
            _           -> MPD.play Nothing)
      ]
  ]
  ++
  [ ((mod1Mask .|. mod, key), spawn script)
    | (mod, key, script) <-
      -- take a window/selected area/entire screen screenshot
      [ (shiftMask,   xK_Print, "upload-screenshot -w")
      , (controlMask, xK_Print, "upload-screenshot -s")
      , (0,           xK_Print, "upload-screenshot -a")
      -- toggle touchpad driver
      , (0,           xK_t, "vaio-touchpad --toggle")
      -- copy selection from work machine
      , (0,           xK_c, "work-copypaste")
      -- screenlock
      , (0,           xK_l, "slock")
      -- love current track in lastfm
      , (0,           xK_f, "lastfm-love-current-mpd-track")
      -- open youtube video in mplayer
      , (0,           xK_y, "youtube-in-mplayer `xsel`")
      -- upload file with copied url to vsegda.budueba.com
      , (0,           xK_u, "upload-budueba `xsel`")
      ]
  ]
  ++
  -- function keys
  [ ((0 .|. mod, key), spawn script)
    | (mod, key, script) <-
      -- sound management
      [ (0,         xF86XK_AudioMute, "vaio-audio --toggle")
      , (0,         xF86XK_AudioRaiseVolume, "vaio-audio --increase")
      , (0,         xF86XK_AudioLowerVolume, "vaio-audio --decrease")
      -- brightness management
      , (shiftMask, xF86XK_AudioMute, "vaio-brightness --toggle")
      , (0,         xF86XK_MonBrightnessDown, "vaio-brightness --decrease")
      , (0,         xF86XK_MonBrightnessUp, "vaio-brightness --increase")
      ]
  ]
--
