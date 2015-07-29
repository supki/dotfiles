{-# LANGUAGE QuasiQuotes #-}
module Work
  ( Work.template
  ) where

import Control.Biegunka (multiline)
import Data.List (intercalate)
import System.Environment
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Defaults


template :: Template
template = Defaults.template
  (\xmobar -> xmobar
    { background = "\"#272727\""
    , position   = "BottomW R 94"
    })
  (\xmonad -> xmonad
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
    })
  (\xmodmap -> xmodmap)
  (\xsession -> xsession
    { setxkbmap = "setxkbmap -option \"\" -layout us,ru -option grp:caps_toggle -option grp_led:scroll :2"
    })
  (\urxvt -> urxvt
    { perllib     = intercalate ":" [ home </> "git/urxvt-tabbedex", home </> "git/urxvt-perls" ]
    , background_ = "#373737"
    , browser     = "x-www-browser"
    })
 where
  home = unsafePerformIO (getEnv "HOME")
