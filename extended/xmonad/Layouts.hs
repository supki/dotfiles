{-# LANGUAGE NoMonomorphismRestriction #-}

module Layouts
  ( myLayoutHook
  ) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane

import qualified Workspaces as W
import Themes

-- Layout hook
myLayoutHook =
  smartBorders $
  avoidStruts $
  onWorkspace (W.toId W.relax) myTiled $
  onWorkspace (W.toId W.chromie) Full $
  onWorkspace (W.toId W.docs) myTabbed $
  onWorkspace (W.toId W.video) Full $
  onWorkspace (W.toId W.status) myTiled $
  onWorkspace (W.toId W.mail) Full $
  onWorkspace (W.toId W.files) Full $
  onWorkspace (W.toId W.torrent) myTiled
  defaultLayout

myTiled   = Tall 1 (3/100) (1/2)
myTwoPane = TwoPane (3/100) (1/2)
myTabbed  = tabbed shrinkText myTheme

defaultLayout = myTwoPane ||| Full
