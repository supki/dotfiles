{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Data.List (intercalate)
import           Data.Maybe (mapMaybe, listToMaybe)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (formatTime, getZonedTime, defaultTimeLocale)
import           Network (PortID(..))
import           Pakej
import qualified Pakej.Widget.Cpu as Cpu
import qualified Pakej.Widget.Memory as Mem
import           Prelude hiding ((.), id)
import           System.Command.QQ (sh)
import           System.IO (withFile, hGetLine, IOMode(..))
import           Text.Printf (printf)


main :: IO ()
main = pakej $ private "all" . aggregate
  [ private "cpu"     . cpu
  , private "mem"     . oom "90%" . mem . every (3 * second)
  , private "ip"      . system [sh| ip.awk eth0 |] . every minute
  , private "battery" . system [sh| bat.rb |] . every (minute `div` 2)
  , private "cputemp" . cputemp . every (10 * second)
  , private "where"   . system [sh| whereami |] . every minute
  , private "weather" . system [sh| weather.rb |] . every minute
  , private "date"    . date . every (minute `div` 2)
  ]

cputemp :: PakejWidget Text
cputemp =
    Text.intercalate " / "
  . mapMaybe (listToMaybe . drop 1 . Text.words <=< Text.stripPrefix "Core")
  . Text.lines <$> system [sh|sensors|]

cpu :: PakejWidget Text
cpu = fmap (fromString . printf "%2.f%%") (Cpu.widget Nothing)

mem :: PakejWidget Text
mem = Mem.widget unknown (fmap percentage . Mem.ratio Mem.used Mem.total)
 where
  percentage = fromString . printf "%2.0f%%" . (100 *)
  unknown    = fromString "??%"

oom :: Text -> Widget IO Text Text Text Text
oom threshold =
  fromWire (mkStateM "00%" (\_ (used, was) ->
    do when (and [used /= was, used > threshold])
            ([sh| notify-send "OOM is coming" "#{used} of memory has already\nbeen used" -i info |] :: IO ())
       return (Right used, used)))
{-# ANN oom "HLint: ignore Use &&" #-}

date :: PakejWidget Text
date = text $ fmap format getZonedTime
 where format       = fromString . formatTime defaultTimeLocale formatString
       formatString = "%m.%d.%y, %a, %H:%M %p"

-- | Read the first line from the file
readLine :: FilePath -> IO String
readLine path = withFile path ReadMode hGetLine
