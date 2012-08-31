module Main (main) where

import Application
import Data.List (intercalate)

main :: IO ()
main = xmobar Config
  { font     = "xft:ubuntu-8"
  , bgColor  = "#333333"
  , fgColor  = "#dd9977"
  , position = Static { xpos = 102, ypos = 750, width = 1264, height = 20 }
  , border = NoBorder
  , borderColor  = "#ff0000"
  , lowerOnStart = True
  , commands =
    [ Run $ Cpu ["-t","<total>","-L","25","-H","25","-l","#eeccaa","--normal","red","--high","red"] 10
    , Run $ Date "%b <fc=#eeccaa>%_d</fc>, (%a), <fc=#eeccaa>%I</fc>:<fc=#eeccaa>%M</fc>" "date" 60
    , Run $ Memory ["-t", "<usedratio>", "-L", "50", "-H", "50", "-l", "#eeccaa", "--normal", "red", "--high", "red"] 10
    , Run $ Network "eth0" ["-t", "<rx>:<tx>", "-L", "512", "-H", "512", "-l", "#eeccaa", "--normal", "red", "--high", "red"] 10
    , Run $ Network "wlan0" ["-t", "<rx>:<tx>", "-L", "512", "-H", "512", "-l", "#eeccaa", "--normal", "red", "--high", "red"] 10
    , Run $ Com "ask-weather" [] "weather" 600
    , Run $ Com "vaio-battery" ["--xmobar"] "battery" 600
    , Run StdinReader
    ]
  , sepChar  = "%"
  , alignSep = "}{"
  , template = intercalate "<fc=#ffffff>|</fc>"
      [ "%StdinReader% }{ %cpu%"
      , " %memory%"
      , " %eth0%"
      , " %wlan0%"
      , " <fc=#eeccaa>%weather%</fc>"
      , " %battery%"
      , " %date%"
      ]
  }
