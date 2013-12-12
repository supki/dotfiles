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
        [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 78 --heighttype pixel --height 17 --tint 0x" ++ tail black ++ " --alpha 0")
        , ("/usr/lib/firefox/firefox", "firefox")
        , ("thunderbird", "thunderbird")
        , ("gnome-commander", "gnome-commander")
        , ("watch -n2 netstat", "urxvt -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
        , ("htop", "urxvt -title work-htop -e htop")
        , ("htop", "urxvt -title dev-htop -e ssh dev -t htop")
        , ("htop", "urxvt -title jenkins-htop -e ssh jenkins -t htop")
        ]
      |]
    , follow    = "True"
    }
  , urxvt = def
    { perllib     = "/home/pyoseek/git/urxvt-tabbedex:/home/pyoseek/git/urxvt-perls"
    , background_ = "#373737"
    , browser     = "firefox"
    }
  }
