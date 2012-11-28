{-# LANGUAGE NoMonomorphismRestriction #-}

module Layouts
  ( myLayoutHook
  ) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

import Workspaces
import Themes

-- Layout hook
myLayoutHook =
  smartBorders $
  avoidStruts $
  onWorkspace WWW Full $
  onWorkspace Texts (tabbed shrinkText myTheme) $
  onWorkspace Video Full $
  onWorkspace Mail Full $
  onWorkspace Files Full $
  onWorkspace Torrents Full $
  onWorkspace Talkative (TwoPane (3/100) (1/2)) $
  Tall 1 0.03 0.5 ||| Full
