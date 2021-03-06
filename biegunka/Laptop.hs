{-# LANGUAGE QuasiQuotes #-}
module Laptop
  ( Laptop.template
  ) where

import Control.Biegunka (multiline)

import Defaults


template :: Template
template = Defaults.template
  (\xmonad -> xmonad
    { terminus  = "xft:terminus:size=12:antialias=true"
    , white     = "#ffffff"
    , darkGray  = "#474747"
    , lightGray = "#cccccc"
    , black     = "#222222"
    , blue      = "#d7d7ff"
    , orange    = "#dd9977"
    , yellow    = "#eeccaa"
    , startup   =
      [multiline|
        [ ("mpd", "mpd")
        , ("(x-www-browser|/usr/lib/chromium/chromium)", "x-www-browser")
        , ("ssh .* mcabber", "urxvtcd -title mcabber -e ssh kolyskovi -t 'tmux attach -d -t mcabber'")
        , ("ssh .* irssi", "urxvtcd -title irssi -e ssh kolyskovi -t 'tmux attach -d -t irssi'")
        , ("watch -n2 netstat", "urxvtcd -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
        , ("htop", "urxvtcd -title htop -e htop")
        , ("procfiled", "procfiled")
        , ("vimus", "urxvtcd -title vimus -name vimus -e zsh -c vimus")
        ]
      |]
    , follow    = "False"
    , patterns  = "[\"git\"]"
    })
  (\xmodmap -> xmodmap
    { menu = "keysym Menu = Super_R"
    })
  (\xsession -> xsession
    { setxkbmap = "setxkbmap -option \"\" -layout us,ua -option ctrl:nocaps -option grp:lctrl_toggle -option grp_led:scroll :2"
    })
  (\urxvt -> urxvt
    { perllib     = "/home/maksenov/git/urxvt-tabbedex:/home/maksenov/git/urxvt-perls"
    , background_ = "#222222"
    })
