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
  , delayed (60 * second) $
      "ip" ~> [sh| ip.awk eth0 |]
  , delayed (30 * second) $
      "date" ~> [sh| date.sh |]
  , delayed (60 * second) $
      "weather" ~> [sh| weather.rb |]
  ]

second :: Int
second = 100000
