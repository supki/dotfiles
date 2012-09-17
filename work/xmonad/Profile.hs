module Profile where

-- Terminal emulator
myTerminal :: String
myTerminal = "urxvt"
--

-- Fonts
ubuntuFont :: String
ubuntuFont = "xft:ubuntu:size=9"
terminusFont :: String
terminusFont = "xft:terminus:size=9"
--

-- Colors
whiteColor :: String
whiteColor = "#ffffff"

grayDarkColor :: String
grayDarkColor = "#515151"

grayLightColor :: String
grayLightColor = "#cccccc"

blackColor :: String
blackColor = "#373737"

orangeColor :: String
orangeColor = "#dd9977"

yellowColor :: String
yellowColor = "#eeccaa"
--

-- Startup apps
apps :: [(String, String)]
apps =
  [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 78 --heighttype pixel --height 17 --tint 0x" ++ tail blackColor ++ " --alpha 0")
  , ("/usr/lib/firefox/firefox", "firefox")
  , ("thunderbird", "thunderbird")
  , ("gnome-commander", "gnome-commander")
  , ("ssh .* im", "urxvt -title IM -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t im'")
  , ("watch -n2 netstat", "urxvt -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
  , ("htop", "urxvt -title htop -e htop")
  ]
--
