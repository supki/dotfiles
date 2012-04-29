module Themes where

import XMonad
import XMonad.Layout.Tabbed
import XMonad.Prompt

import qualified Profile         as P

-- Theme
myTheme = defaultTheme {
  activeColor = P.blackColor,
  inactiveColor = P.grayDarkColor,
  activeBorderColor = P.yellowColor,
  inactiveBorderColor = P.orangeColor,
  activeTextColor = P.yellowColor,
  inactiveTextColor = P.orangeColor,
  fontName = P.ubuntuFont,
  decoWidth = 1378,
  decoHeight = 20
}
--

-- XPConfig
myXPConfig = defaultXPConfig {
  font = P.terminusFont,
  bgColor = P.blackColor,
  borderColor = P.yellowColor,
  fgColor = P.yellowColor,
  position = Top,
  height = 20,
  autoComplete = Just 1
}
--

