{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Control.Applicative
import           Data.Foldable (asum)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (intercalate)
import           Data.Maybe (fromMaybe)
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Data.Time (formatTime, getZonedTime)
import           Data.Traversable (sequenceA)
import           Network (PortID(..))
import           Pakej
import           Pakej.Widget.Memory
import           Prelude hiding ((.), id)
import           System.Command.QQ (sh)
import           System.Locale (defaultTimeLocale)
import           System.IO (withFile, hGetLine, IOMode(..))
import           Text.Printf (printf)


main :: IO ()
main = pakej $ private "all" . aggregate
  [ private "cpu"       . cpu "/proc/stat"
  , private "mem"       . mem "/proc/meminfo"
  , private "ip"        . system [sh| ip.awk eth0 |] . every minute
  , private "battery"   . system [sh| bat.rb |] . every (minute `div` 2)
  , private "loadavg"   . loadavg "/proc/loadavg" . every (10 * second)
  , private "loadavg2"  . query "budueba.com" (PortNumber 1234) "loadavg" . every minute
  , private "weather"   . system [sh| weather.rb |] . every minute
  , private "playcount" . system [sh| playcount |] . every minute
  , private "date"      . date . every (minute `div` 2)
  ]

cpu :: FilePath -> PakejWidget Text
cpu path = fromWire (fmap format compute) . constant (cpuData path)
 where cpuData = fmap (drop 1 . map read . words) . readLine
       compute = mkState (repeat 0) $ \_dt (v, s) ->
         let
           d = zipWith (-) v s
         in case sum d of
             t | abs t < 0.001 -> (Left undefined, s)
               | otherwise     -> (Right (sum (map (/ t) (take 3 d)) * 100 :: Double), v)
       format  = fromString . printf "%2.f%%"

mem :: FilePath -> PakejWidget Text
mem path = text $
  memUsage <$> memoryData path
 where
  memUsage (Left _) = unknown
  memUsage (Right xs) = fromMaybe unknown $ asum
    [ liftA2 usage available total
    , liftA2 usage (fmap sum (sequenceA [free, buffers, cached])) total
    ]
   where
    total      = HashMap.lookup "MemTotal" xs
    free       = HashMap.lookup "MemFree" xs
    available  = HashMap.lookup "MemAvailable" xs
    buffers    = HashMap.lookup "Buffers" xs
    cached     = HashMap.lookup "Cached" xs

    usage x y  = format (100.0 * (fromIntegral y - fromIntegral x) / fromIntegral y :: Double)
    format     = fromString . printf "%2.0f%%"

  unknown    = fromString "??%"

loadavg :: FilePath -> PakejWidget Text
loadavg path = text $ fromString . intercalate " → " . take 3 . words <$> readFile path

date :: PakejWidget Text
date = text $ fmap format getZonedTime
 where format       = fromString . formatTime defaultTimeLocale formatString
       formatString = "%m.%d.%y, %a, %H:%M %p"

-- | Read the first line from the file
readLine :: FilePath -> IO String
readLine path = withFile path ReadMode hGetLine
