{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Applicative
import Data.List (intercalate)
import Data.String (IsString(..))
import Data.Time (formatTime, getZonedTime)
import System.Locale (defaultTimeLocale)
import Prelude hiding ((.), id)
import Pakej
import System.Command.QQ (sh)


main :: IO ()
main = pakej $ private "all" . aggregate
  [ private "cpu"       . system [sh| cpu.pl /proc/stat |] . every (5 * second)
  , private "mem"       . system [sh| mem.awk /proc/meminfo |] . every (10 * second)
  , private "ip"        . system [sh| ip.awk eth0 |] . every minute
  , private "battery"   . system [sh| bat.rb |] . every (minute `div` 2)
  , private "loadavg"   . text (loadavg "/proc/loadavg") . every (10 * second)
  , private "loadavg2"  . system [sh| pakej -h budueba.com -p 1234 loadavg |] . every minute
  , private "weather"   . system [sh| weather.rb |] . every minute
  , private "playcount" . system [sh| playcount |] . every minute
  , private "date"      . text date . every (minute `div` 2)
  ]

loadavg :: IsString s => FilePath -> IO s
loadavg path = fromString . intercalate " → " . take 3 . words <$> readFile path

date :: IsString s => IO s
date = fmap format getZonedTime
 where format = fromString . formatTime defaultTimeLocale formatString
       formatString = "%m.%d.%y, %a, %H:%M %p"
