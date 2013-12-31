{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Pakej
import System.Command.QQ (sh)


main :: IO ()
main = pakej
  [ run $ "cpu"     ~> [sh| cpu.pl /proc/stat |]
  , run $ "mem"     ~> [sh| mem.awk /proc/meminfo |]
  , run $ "battery" ~> [sh| bat.rb |]
  , run $ "loadavg" ~> [sh| loadavg.awk /proc/loadavg |]
  , run . delay (60 *) $
      "ip" ~> [sh| ip.awk eth0 |]
  , run . delay (30 *) $
      "date" ~> [sh| date.sh |]
  , run . delay (60 *) $
      "playcount" ~> [sh| playcount |]
  , run . delay (60 *) $
      "loadavg2" ~> [sh| pakej --hostname budueba.com --port 1234 loadavg |]
  , run . delay (60 *) $
      "weather" ~> [sh| weather.rb |]
  , run $
      "all" |> ["cpu", "mem", "ip", "battery", "loadavg", "loadavg2", "weather", "playcount", "date"]
  ]
