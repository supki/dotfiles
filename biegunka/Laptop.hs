{-# LANGUAGE QuasiQuotes #-}
module Laptop where

import Data.Default (def)
import Data.String.Quote

import Defaults


templates :: Template
templates = def
  { xmobar = def
    { background = "\"#222222\""
    , position   = "Static { xpos       = 102, ypos = 750, width = 1264, height = 20 }"
    , battery    = Just "\"%battery%\""
    }
  , xmonad = def
    { terminal  = "urxvtcd"
    , ubuntu    = "xft:ubuntu:size   = 9"
    , terminus  = "xft:terminus:size = 9"
    , white     = "#ffffff"
    , grayDark  = "#474747"
    , grayLight = "#cccccc"
    , black     = "#222222"
    , orange    = "#dd9977"
    , yellow    = "#eeccaa"
    , startup   =
      [s|
        [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 102 --heighttype pixel --height 18 --tint 0x" ++ tail blackColor ++ " --alpha 0")
        , ("/home/maksenov/.dropbox-dist/dropbox", "${HOME}/.dropbox-dist/dropboxd")
        , ("mpd", "mpd")
        , ("x-www-browser", "x-www-browser")
        , ("gnome-commander", "gnome-commander")
        , ("transmission-gtk", "transmission-gtk")
        , ("ssh .* mcabber", "urxvtcd -title mcabber -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -d -t mcabber'")
        , ("ssh .* irssi", "urxvtcd -title irssi -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -d -t irssi'")
        , ("/usr/bin/ruby1.9.1 /usr/local/bin/poneaux", "urxvtcd -title poneaux -e poneaux")
        , ("watch -n2 netstat", "urxvtcd -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
        , ("htop", "urxvtcd -title htop -e htop")
        , ("scrobbler-client", "scrobbler-client")
        , ("procfiled", "procfiled")
        , ("icedove", "icedove")
        , ("suspender", "suspender")
        ]
      |]
    , follow    = "False"
    }
  , xmodmap = def
    { menu = "keysym Menu = Super_R"
    }
  , urxvt = def
    { perllib     = "/home/maksenov/git/urxvt-tabbedex:/home/maksenov/git/urxvt-perls"
    , background_ = "#222222"
    , browser     = "iceweasel"
    }
  }
