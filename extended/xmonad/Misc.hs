module Misc where

import XMonad

import qualified Profile         as P

-- Terminal emulator
myTerminal = "urxvtc"
--

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
--

-- Modificator masks
myModMask :: KeyMask
myModMask = mod4Mask
myNumlockMask :: KeyMask
myNumlockMask = mod2Mask
--

-- Borders
myBorderWidth :: Dimension
myBorderWidth = 1
myNormalBorderColor :: String
myNormalBorderColor = P.grayDarkColor
myFocusedBorderColor :: String
myFocusedBorderColor = P.yellowColor
--

