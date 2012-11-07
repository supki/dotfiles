module Profile where

-- Startup apps
apps :: [(String, String)]
apps =
  [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 78 --heighttype pixel --height 17 --tint 0x" ++ tail blackColor ++ " --alpha 0")
  , ("/usr/lib/firefox/firefox", "firefox")
  , ("thunderbird", "thunderbird")
  , ("gnome-commander", "gnome-commander")
  , ("ssh .* mcabber", "urxvtcd -title mcabber -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t mcabber'")
  , ("ssh .* irssi", "urxvtcd -title irssi -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t irssi'")
  , ("watch -n2 netstat", "urxvt -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
  , ("htop", "urxvt -title htop -e htop")
  ]
--
