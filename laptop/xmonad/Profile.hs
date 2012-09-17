module Profile where

import Misc

-- Startup apps
apps :: [(String, String)]
apps =  [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 102 --heighttype pixel --height 18 --tint 0x" ++ tail blackColor ++ " --alpha 0")
  , ("/home/maksenov/.dropbox-dist/dropbox", "${HOME}/.dropbox-dist/dropboxd")
  , ("mpd", "mpd")
  , ("/usr/lib/iceweasel", "iceweasel")
  , ("gnome-commander", "gnome-commander")
  , ("transmission-gtk", "transmission-gtk")
  , ("ssh .* im", "urxvtcd -title IM -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t im'")
  , ("watch -n2 netstat", "urxvtcd -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
  , ("htop", "urxvtcd -title htop -e htop")
  , ("liblastfm-scrobbler", "liblastfm-scrobbler")
  , ("procfiled", "procfiled")
  , ("icedove", "icedove")
  , ("python /home/maksenov/bin/trayicon-mpd", "trayicon-mpd")
  , ("python /home/maksenov/bin/trayicon-mcabber", "trayicon-mcabber")
  , ("suspender", "suspender")
  ]
--
