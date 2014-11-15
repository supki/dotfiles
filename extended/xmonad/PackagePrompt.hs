{-# LANGUAGE OverloadedStrings #-}
module PackagePrompt
  ( packagePrompt
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List (group, isPrefixOf)
import           Data.Maybe (mapMaybe, listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))
import           System.IO.Error (catchIOError)
import           XMonad (X, liftIO)
import           XMonad.Prompt (XPrompt(..), XPType(..), XPConfig, mkXPromptWithModes)
import           XMonad.Util.Run (safeSpawn)

data Hackage = Hackage [String]

instance XPrompt Hackage where
  showXPrompt _ = "hackage: "
  modeAction _ q c =
    safeSpawn "x-www-browser" . pure . package . bool c q $ c == noPackages || null c
  completionFunction (Hackage [p])  _
    | p == noPackages                   = return [p]
  completionFunction (Hackage _)    ""  = return []
  completionFunction (Hackage ps)   str =
    return . filter (str `isPrefixOf`) $ ps

package :: String -> String
package p = "https://hackage.haskell.org/package" </> p

packagePrompt :: XPConfig -> X ()
packagePrompt conf = do
  ps <- liftIO packages
  mkXPromptWithModes [XPT (Hackage ps)] conf

packages :: IO [String]
packages = do
  db <- cabalCache
  fmap Text.unpack . heads . mapMaybe (firstWord <=< Text.stripPrefix "pkg:") . Text.lines <$> Text.readFile db
 `catchIOError`
  \_ -> return [noPackages]

firstWord :: Text -> Maybe Text
firstWord = listToMaybe . Text.words

heads :: Eq a => [a] -> [a]
heads = map head . group

noPackages :: String
noPackages = "Please run ‘cabal update’ for the prompt completions to appear"

cabalCache :: IO FilePath
cabalCache = fmap (</> ".cabal/packages/hackage.haskell.org/00-index.cache") home

home :: IO FilePath
home = getHomeDirectory

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
