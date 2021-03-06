module Themes where

import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           XMonad
import           XMonad.Layout.Tabbed
import           XMonad.Prompt

import           Profile

myTheme :: Theme
myTheme = defaultTheme
  { activeColor         = black
  , inactiveColor       = darkGray
  , activeBorderColor   = yellow
  , inactiveBorderColor = orange
  , activeTextColor     = yellow
  , inactiveTextColor   = orange
  , font                = terminusFont
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
  , promptKeymap = promptCustomKeymap <> promptKeymap defaultXPConfig
  }

type PromptKeymap = Map (KeyMask, KeySym) (XP ())

promptCustomKeymap :: PromptKeymap
promptCustomKeymap = Map.singleton (controlMask, xK_j) (do setSuccess True; setDone True)
