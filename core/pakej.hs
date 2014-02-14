{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Control.Applicative
import Control.Wire (mkState)
import Data.List (intercalate)
import Data.String (IsString(..))
import Data.Time (formatTime, getZonedTime)
import Data.Text.Lazy (Text)
import System.Locale (defaultTimeLocale)
import System.IO (withFile, hGetLine, IOMode(..))
import Prelude hiding ((.), id)
import Pakej
import System.Command.QQ (sh)
import Text.Printf (printf)


main :: IO ()
main = pakej $ private "all" . aggregate
  [ private "cpu"       . cpu "/proc/stat"
  , private "mem"       . system [sh| mem.awk /proc/meminfo |] . every (10 * second)
  , private "ip"        . system [sh| ip.awk eth0 |] . every minute
  , private "battery"   . system [sh| bat.rb |] . every (minute `div` 2)
  , private "loadavg"   . loadavg "/proc/loadavg" . every (10 * second)
  , private "loadavg2"  . system [sh| pakej -h budueba.com -p 1234 loadavg |] . every minute
  , private "weather"   . system [sh| weather.rb |] . every minute
  , private "playcount" . system [sh| playcount |] . every minute
  , private "date"      . date . every (minute `div` 2)
  ]

cpu :: FilePath -> PakejWidget Text
cpu path = fmap format compute . constant (cpuData path)
 where cpuData = fmap (drop 1 . map read . words) . readLine

       compute = mkState (repeat 0) $ \_dt (v, s) ->
         let
           d = zipWith (-) v s
         in case sum d of
             t | abs t < 0.001 -> (Left undefined, s)
               | otherwise     -> (Right (sum (map (/ t) (take 3 d)) * 100 :: Double), v)

       format  = fromString . printf "%2.f%%"

loadavg :: FilePath -> PakejWidget Text
loadavg path = text $ fromString . intercalate " → " . take 3 . words <$> readFile path

date :: PakejWidget Text
date = text $ fmap format getZonedTime
 where format       = fromString . formatTime defaultTimeLocale formatString
       formatString = "%m.%d.%y, %a, %H:%M %p"

-- | Read the first line from the file
readLine :: FilePath -> IO String
readLine path = withFile path ReadMode hGetLine
