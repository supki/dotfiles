{-# LANGUAGE QuasiQuotes #-}
module Work
  ( Work.template
  ) where

import Control.Biegunka (multiline)
import System.Environment
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import Defaults


{-# NOINLINE template #-}
{-# ANN template "HLint: ignore Use id" #-}
template :: Template
template = Defaults.template
  (\xmonad -> xmonad
    { terminus  = "xft:terminus:size=8"
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
    { perllib     = home </> "git/urxvt-perls"
    , background_ = "#222222"
    })
 where
  home = unsafePerformIO (getEnv "HOME")
