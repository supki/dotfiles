module Profile where

import XMonad

-- Startup apps
apps :: [(String, String)]
apps = $template.xmonad.startup$

patterns :: [String]
patterns = $template.xmonad.patterns$

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = $template.xmonad.follow$
--

-- Whether not pass focusing click to window
myClickJustFocuses :: Bool
myClickJustFocuses = False
--

-- Modificator masks
myModMask :: KeyMask
myModMask = mod4Mask
myNumlockMask :: KeyMask
myNumlockMask = mod2Mask
--

-- Borders
myBorderWidth :: Dimension
myBorderWidth = 2
myNormalBorderColor :: String
myNormalBorderColor = darkGray
myFocusedBorderColor :: String
myFocusedBorderColor = blue
--

-- Terminal emulator
myTerminal :: String
myTerminal = "$template.xmonad.terminal$"
--

-- Fonts
ubuntuFont :: String
ubuntuFont = "$template.xmonad.ubuntu$"
terminusFont :: String
terminusFont = "$template.xmonad.terminus$"
--

-- Colors
white :: String
white = "$template.xmonad.white$"

darkGray :: String
darkGray = "$template.xmonad.darkGray$"

lightGray :: String
lightGray = "$template.xmonad.lightGray$"

black :: String
black = "$template.xmonad.black$"

orange :: String
orange = "$template.xmonad.orange$"

yellow :: String
yellow = "$template.xmonad.yellow$"

blue :: String
blue = "$template.xmonad.blue$"
--
