module Themes where

import XMonad.Layout.Tabbed
import XMonad.Prompt

import Profile

-- Theme
myTheme :: Theme
myTheme = defaultTheme {
  activeColor = blackColor,
  inactiveColor = grayDarkColor,
  activeBorderColor = yellowColor,
  inactiveBorderColor = orangeColor,
  activeTextColor = yellowColor,
  inactiveTextColor = orangeColor,
  fontName = ubuntuFont,
  decoWidth = 1378,
  decoHeight = 20
}
--

-- XPConfig
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {
  font = terminusFont,
  bgColor = blackColor,
  borderColor = yellowColor,
  fgColor = yellowColor,
  position = Top,
  height = 20,
  autoComplete = Nothing
}
--

