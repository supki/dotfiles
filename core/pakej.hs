{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Pakej
import System.Command.QQ (sh)


main :: IO ()
main = pakej
  [ "cpu"     ~> [sh| cpu.pl /proc/stat |]
  , "mem"     ~> [sh| mem.awk /proc/meminfo |]
  , "battery" ~> [sh| bat.rb |]
  , "loadavg" ~> [sh| loadavg.awk /proc/loadavg |]
  , delayed (60 * defaultTimeout) $
      "ip" ~> [sh| ip.awk eth0 |]
  , delayed (30 * defaultTimeout) $
      "date" ~> [sh| date.sh |]
  , delayed (60 * defaultTimeout) $
      "weather" ~> [sh| weather.rb |]
  , "all" |> ["cpu", "mem", "ip", "battery", "weather", "date", "loadavg"]
  ]
