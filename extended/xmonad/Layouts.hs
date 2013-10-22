{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -W #-}
module Layouts
  ( myLayoutHook
  ) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.DwmStyle
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

import Themes
import Workspaces


myLayoutHook = smartBorders $ avoidStruts $
  onWorkspace Texts (tabbed shrinkText myTheme) $
  onWorkspaces [Video, Mail, Files, Torrents] Full $
  onWorkspace Status (dwmStyle shrinkText myTheme tall) $
  onWorkspace Talkative (TwoPane (3/100) (1/2)) $
  tall ||| Mirror tall |||  Full
 where
  tall = Tall 1 0.03 0.5
