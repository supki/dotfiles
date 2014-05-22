{-# LANGUAGE QuasiQuotes #-}
module Work where

import Control.Biegunka (multiline)
import Data.Default (def)

import Defaults


templates :: Template
templates = def
  { xmobar = def
    { background = "\"#373737\""
    , position   = "BottomW R 94"
    }
  , xmonad = def
    { terminal  = "urxvt"
    , ubuntu    = "xft:ubuntu:size   = 9"
    , terminus  = "xft:terminus:size = 9"
    , white     = "#ffffff"
    , darkGray  = "#515151"
    , lightGray = "#cccccc"
    , black     = "#373737"
    , blue      = "#d7d7ff"
    , orange    = "#dd9977"
    , yellow    = "#eeccaa"
    , startup   =
      [multiline|
        [ ("/usr/lib/firefox/firefox", "firefox")
        , ("gnome-commander", "gnome-commander")
        , ("watch -n2 netstat", "urxvt -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
        , ("htop", "urxvt -title work-htop -e htop")
        , ("htop", "urxvt -title dev-htop -e ssh dev -t htop")
        , ("htop", "urxvt -title jenkins-htop -e ssh jenkins -t htop")
        ]
      |]
    , follow    = "True"
    , patterns  = "[\"git/*\", \"svn/*\"]"
    }
  , xsession = def
    { setxkbmap = "setxkbmap -option \"\" -layout us,ru -option grp:caps_toggle -option grp_led:scroll :2"
    }
  , urxvt = def
    { perllib     = "/home/pyoseek/git/urxvt-tabbedex:/home/pyoseek/git/urxvt-perls"
    , background_ = "#373737"
    , browser     = "firefox"
    }
  }
