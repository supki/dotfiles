{-# OPTIONS_GHC -W #-}
module Themes where

import XMonad.Layout.Tabbed
import XMonad.Prompt

import Profile

myTheme :: Theme
myTheme = defaultTheme
  { activeColor         = black
  , inactiveColor       = darkGray
  , activeBorderColor   = yellow
  , inactiveBorderColor = orange
  , activeTextColor     = yellow
  , inactiveTextColor   = orange
  , fontName            = ubuntuFont
  , decoWidth           = 1378
  , decoHeight          = 20
  }

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { font         = terminusFont
  , bgColor      = black
  , borderColor  = yellow
  , fgColor      = yellow
  , position     = Top
  , height       = 20
  , autoComplete = Nothing
  }
