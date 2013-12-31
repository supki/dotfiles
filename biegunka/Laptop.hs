{-# LANGUAGE QuasiQuotes #-}
module Laptop where

import Control.Biegunka (multiline)
import Data.Default (def)

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
    , darkGray  = "#474747"
    , lightGray = "#cccccc"
    , black     = "#222222"
    , blue      = "#d7d7ff"
    , orange    = "#dd9977"
    , yellow    = "#eeccaa"
    , startup   =
      [multiline|
        [ ("/home/maksenov/.dropbox-dist/dropbox", "${HOME}/.dropbox-dist/dropboxd")
        , ("mpd", "mpd")
        , ("(x-www-browser|/usr/lib/chromium/chromium)", "x-www-browser")
        , ("gnome-commander", "gnome-commander")
        , ("transmission-gtk", "transmission-gtk")
        , ("ssh .* mcabber", "urxvtcd -title mcabber -e ssh budueba -t 'export LANG=en_US.UTF-8; tmux attach -d -t mcabber'")
        , ("ssh .* irssi", "urxvtcd -title irssi -e ssh budueba -t 'export LANG=en_US.UTF-8; tmux attach -d -t irssi'")
        , ("/usr/bin/ruby1.9.1 poneaux", "urxvtcd -title poneaux -e poneaux")
        , ("watch -n2 netstat", "urxvtcd -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
        , ("htop", "urxvtcd -title htop -e htop")
        , ("scrobbler-client", "scrobbler-client")
        , ("procfiled", "procfiled")
        , ("suspender", "suspender")
        , ("vimus", "urxvtcd -title vimus -name vimus -e zsh -c vimus")
        ]
      |]
    , follow    = "False"
    }
  , xmodmap = def
    { menu = "keysym Menu = Super_R"
    }
  , xsession = def
    { setxkbmap = "setxkbmap -option \"\" -layout us,ru -option ctrl:nocaps -option grp:lctrl_toggle -option grp_led:scroll :2"
    }
  , urxvt = def
    { perllib     = "/home/maksenov/git/urxvt-tabbedex:/home/maksenov/git/urxvt-perls"
    , background_ = "#222222"
    , browser     = "x-www-browser"
    }
  }
