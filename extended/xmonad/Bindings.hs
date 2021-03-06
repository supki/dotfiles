{-# LANGUAGE CPP #-}
module Bindings where

import           Control.Applicative
import           Control.Monad
import           Data.Map (Map)
import           Data.Foldable (traverse_)
import qualified Data.Map as M
import           Graphics.X11.ExtraTypes.XF86
#ifdef HAS_VIMUS
import qualified Network.MPD as MPD
#endif
import           Prelude hiding (mod)
import           XMonad hiding (spawn)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.UseEmptyWorkspace
import           XMonad.Hooks.ManageDocks
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import qualified Profile
import qualified Workspaces as WS
import           Spawn
import qualified Startup
import           Themes
import qualified Tmux
import           PackagePrompt (packagePrompt)



myMouseBindings :: XConfig t -> Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
  -- move the window
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w)
  -- resize the window
  , ((modm, button3), \w -> focus w >> mouseResizeWindow w)
  ]


myKeyboardBindings :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeyboardBindings conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  -- default
  [ ((modm .|. mod, key), command)
    | (mod, key, command) <-
      -- launch a terminal
      [ (shiftMask,   xK_Return, spawn $ XMonad.terminal conf)
      -- launch tmux prompt
      , (0,           xK_Return, Tmux.prompt Profile.patterns myXPConfig)
      -- launch packagePrompt
      , (0,           xK_o, packagePrompt myXPConfig)
      -- launch shellPrompt
      , (0,           xK_p, shellPrompt myXPConfig)
      -- close focused window
      , (shiftMask,   xK_c, kill)
      -- rotate through the available layout algorithms
      , (0,           xK_space, sendMessage NextLayout)
      -- rotate through the available screens
      , (shiftMask,   xK_space, swapNextScreen)
      -- resize viewed windows to the correct size
      , (0,           xK_n, refresh)
      -- toggle scratchpads
      , (0,           xK_s, namedScratchpadAction WS.scratchpads "scratchpad")
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
      -- do stuff with empty workspaces
      , (0,           xK_a, withEmptyWorkspace real (windows . W.view))
      , (controlMask, xK_a, withEmptyWorkspace real (\w -> windows $ W.view w . W.shift w))
      -- push window back into tiling
      , (0,           xK_t, withFocused $ windows . W.sink)
      -- increment/decrement the number of windows in the master area
      , (0,           xK_comma, sendMessage (IncMasterN 1))
      , (0,           xK_period, sendMessage (IncMasterN (-1)))
      -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
      , (0,           xK_b, sendMessage ToggleStruts)
      -- rerun the startup hook and reset layouts
      , (0,           xK_q, do
          Startup.myStartupHook
          setLayout (XMonad.layoutHook conf))
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
      [ (0,           xK_w, nextScreen)
      , (shiftMask,   xK_w, shiftNextScreen)
      ]
      ++
      -- swap workspaces and return to old one
      [ (controlMask, k, withWindowSet (\s -> windows (swapWithCurrent i) >> windows (W.greedyView $ W.currentTag s)))
        | (i, k) <- zip WS.myWorkspaces WS.myWorkspacesKeys
      ]
  ]
#ifdef HAS_VIMUS
  ++
  -- mpd
  [ ((mod1Mask .|. mod, key), script)
    | (mod, key, script) <-
      [ (shiftMask, xK_comma,  io_ $ MPD.withMPD MPD.previous)
      , (shiftMask, xK_period, io_ $ MPD.withMPD MPD.next)
      , (shiftMask, xK_Return, io_ . MPD.withMPD $
          MPD.status >>= \s -> case MPD.stState s of
            MPD.Playing -> MPD.pause True
            _           -> MPD.play Nothing)
      , (shiftMask, xK_minus, io_ . MPD.withMPD $ do
          st <- MPD.status
          let v = MPD.stVolume st
              nv = max 0 (v - 5)
          MPD.setVolume nv)
      , (shiftMask, xK_equal, io_ . MPD.withMPD $ do
          st <- MPD.status
          let v = MPD.stVolume st
              nv = min 100 (v + 5)
          MPD.setVolume nv)
      ]
  ]
#endif
  ++
  -- third party scripts
  [ ((mod1Mask .|. mod, key), spawn script)
    | (mod, key, script) <-
      -- screenlock
      [ (0,           xK_l, "slock")
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
      , (0,         xF86XK_MonBrightnessDown, "vaio-brightness --decrease")
      , (0,         xF86XK_MonBrightnessUp, "vaio-brightness --increase")
      ]
  ]


real :: WindowSpace -> Maybe WorkspaceId
real w = let tag = W.tag w in tag <$ guard (tag /= "NSP")

io_ :: MonadIO m => IO a -> m ()
io_ = io . void
