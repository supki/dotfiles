module Profile where

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
grayDarkColor = "#474747"

grayLightColor :: String
grayLightColor = "#cccccc"

blackColor :: String
blackColor = "#333333"

orangeColor :: String
orangeColor = "#dd9977"

yellowColor :: String
yellowColor = "#eeccaa"
--

-- Startup apps
apps :: [(String, String)]
apps =  [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 102 --heighttype pixel --height 18 --tint 0x" ++ tail blackColor ++ " --alpha 0")
  , ("/home/maksenov/.dropbox-dist/dropbox", "${HOME}/.dropbox-dist/dropboxd")
  , ("mpd", "mpd")
  , ("/usr/lib/iceweasel", "iceweasel")
  , ("gnome-commander", "gnome-commander")
  , ("transmission-gtk", "transmission-gtk")
  , ("ssh .* IM", "urxvtc -title IM -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t im'")
  , ("watch -n2 netstat", "urxvtc -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
  , ("htop", "urxvtc -title htop -e htop")
  , ("urxvtd", "urxvtd -q -f -o")
  , ("liblastfm-scrobbler", "liblastfm-scrobbler")
  , ("procfiled", "procfiled")
  , ("icedove", "icedove")
  , ("python /home/maksenov/bin/trayicon-mpd", "trayicon-mpd")
  , ("python /home/maksenov/bin/trayicon-mcabber", "trayicon-mcabber")
  ]
--

