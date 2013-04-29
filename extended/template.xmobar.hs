module Main (main) where

import Data.List (intercalate)

import Application


main :: IO ()
main = xmobar Config
  { font = "xft:ubuntu-8"
  , bgColor  = $template.xmobar.background$
  , fgColor = "#dd9977"
  , border = NoBorder
  , borderColor  = "#ff0000"
  , position = $template.xmobar.position$
  , lowerOnStart = False
  , hideOnStart = False
  , persistent = False
  , commands =
    [ Run (Cpu ["-t","<total>","-L","25","-H","25","-l","#eeccaa","--normal","red","--high","red"] 10)
    , Run (Memory ["-t", "<usedratio>", "-L", "50", "-H", "50", "-l", "#eeccaa", "--normal", "red", "--high", "red"] 10)
    , Run (Network "eth0" ["-t", "<rx>:<tx>", "-L", "512", "-H", "512", "-l", "#eeccaa", "--normal", "red", "--high", "red"] 10)
    , Run (Network "wlan0" ["-t", "<rx>:<tx>", "-L", "512", "-H", "512", "-l", "#eeccaa", "--normal", "red", "--high", "red"] 10)
    , Run (Com "ask-weather" [] "weather" 600)
    , Run (Com "pemised" [] "mypemi" 60)
    , Run (Com "playcount" [] "mypc" 60)
    , Run (Com "vaio-battery" ["--xmobar"] "battery" 600)
    , Run (Com "date"
        ["+'%b <fc=#eeccaa>%d</fc>, (%a), <fc=#eeccaa>%I</fc>:<fc=#eeccaa>%M</fc> %P'"]
        "mydate"
        600)
    , Run StdinReader
    ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = intercalate sep
      [ " %StdinReader% }{ %cpu%"
      , "%memory%"
      , "%eth0%"
      , "%wlan0%"
      , "<fc=#eeccaa>%myweather%</fc>"
      , "%mypemi%"
      , "<fc=#eeccaa>%mypc%</fc>"
      , $template.xmobar.battery$
      , "%mydate% "
      ]
  }


sep = "<fc=#ffffff>|</fc>"
