{-# LANGUAGE QuasiQuotes #-}
module Work where

import Data.Default (def)
import Data.String.Quote

import Defaults


templates :: Template
templates = def
  { xmobar = def
    { background = "\"#373737\""
    , position = "BottomW R 94"
    }
  , xmonad = def
    { terminal = "urxvt"
    , ubuntu = "xft:ubuntu:size=9"
    , terminus = "xft:terminus:size=9"
    , white = "#ffffff"
    , grayDark = "#515151"
    , grayLight = "#cccccc"
    , black = "#373737"
    , orange = "#dd9977"
    , yellow = "#eeccaa"
    , startup =
      [s|
        [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 78 --heighttype pixel --height 17 --tint 0x" ++ tail blackColor ++ " --alpha 0")
        , ("/usr/lib/firefox/firefox", "firefox")
        , ("thunderbird", "thunderbird")
        , ("gnome-commander", "gnome-commander")
        , ("ssh .* mcabber", "urxvt -title mcabber -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; TERM=rxvt tmux attach -t mcabber'")
        , ("ssh .* irssi", "urxvt -title irssi -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; TERM=rxvt tmux attach -t irssi'")
        , ("watch -n2 netstat", "urxvt -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
        , ("htop", "urxvt -title work-htop -e htop")
        , ("htop", "urxvt -title dev-htop -e ssh dev -t htop")
        , ("htop", "urxvt -title jenkins-htop -e ssh jenkins -t htop")
        ]
      |]
    , follow = "True"
    }
  , urxvt = def
    { perllib = "/home/pyoseek/git/urxvt-tabbedex:/home/pyoseek/git/urxvt-perls"
    , background_ = "#373737"
    , browser = "firefox"
    }
  }
