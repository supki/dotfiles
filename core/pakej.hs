{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Pakej
import System.Command.QQ (sh)


main :: IO ()
main = pakej
  [ every  5 $ "cpu"       ~> [sh| cpu.pl /proc/stat |]
  , every 10 $ "mem"       ~> [sh| mem.awk /proc/meminfo |]
  , every 30 $ "battery"   ~> [sh| bat.rb |]
  , every 10 $ "loadavg"   ~> [sh| loadavg.awk /proc/loadavg |]
  , every 60 $ "ip"        ~> [sh| ip.awk eth0 |]
  , every 30 $ "date"      ~> [sh| date.sh |]
  , every 60 $ "playcount" ~> [sh| playcount |]
  , every 60 $ "loadavg2"  ~> [sh| pakej --hostname budueba.com --port 1234 loadavg |]
  , every 60 $ "weather"   ~> [sh| weather.rb |]
  , run $
      "all" |> ["cpu", "mem", "ip", "battery", "loadavg", "loadavg2", "weather", "playcount", "date"]
  ]

every :: Int -> Pakejee IO a -> Pakej a
every n = run . delay (n *)
