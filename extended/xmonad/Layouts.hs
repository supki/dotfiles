{-# LANGUAGE NoMonomorphismRestriction #-}

module Layouts
  ( myLayoutHook
  ) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed

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
  GridRatio (3/2) ||| Full
