{-# LANGUAGE QuasiQuotes #-}
module Work where

import Control.Biegunka (multiline)
import Data.Default.Class (def)

import Defaults


templates :: Template
templates = def
  { xmobar = def
    { background = "\"#373737\""
    , position   = "BottomW R 94"
    }
  , xmonad = def
    { terminal  = "urxvt"
    , ubuntu    = "xft:ubuntu:size=8"
    , terminus  = "xft:terminus:size=8"
    , white     = "#ffffff"
    , darkGray  = "#515151"
    , lightGray = "#cccccc"
    , black     = "#373737"
    , blue      = "#d7d7ff"
    , orange    = "#dd9977"
    , yellow    = "#eeccaa"
    , startup   =
      [multiline|
        [ ("(x-www-browser|/usr/lib/chromium/chromium|/usr/lib/chromium-browser/chro)", "x-www-browser")
        , ("htop", "urxvt -title work-htop -e htop")
        , ("jm", "urxvt -title jm -e sh -c '. ~/.j/j.env; jm'")
        ]
      |]
    , follow    = "True"
    , patterns  = "[\"git\", \"svn\"]"
    }
  , xsession = def
    { setxkbmap = "setxkbmap -option \"\" -layout us,ru -option grp:caps_toggle -option grp_led:scroll :2"
    }
  , urxvt = def
    { perllib     = "/home/pyoseek/git/urxvt-tabbedex:/home/pyoseek/git/urxvt-perls"
    , background_ = "#373737"
    , browser     = "x-www-browser"
    }
  }
