module PackagePrompt
  ( packagePrompt
  ) where

import           Control.Applicative
import           Data.Bool (bool)
import           Data.List (group, isPrefixOf)
import           Data.Maybe (mapMaybe, listToMaybe)
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
  modeAction _ q c = safeSpawn "x-www-browser" [package (bool c q (null c))]
  completionFunction (Hackage _)  ""  = return []
  completionFunction (Hackage ps) str =
    return . heads . filter (str `isPrefixOf`) $ ps

package :: String -> String
package p = "https://hackage.haskell.org/package" </> p

heads :: Eq a => [a] -> [a]
heads = map head . group

packagePrompt :: XPConfig -> X ()
packagePrompt conf = do
  ps <- liftIO packages
  mkXPromptWithModes [XPT (Hackage ps)] conf

packages :: IO [String]
packages = do
  db <- cabalCache
  mapMaybe (fmap Text.unpack . listToMaybe . drop 1 . Text.words) . Text.lines <$> Text.readFile db
 `catchIOError`
  \_ -> return []

cabalCache :: IO FilePath
cabalCache = fmap (</> ".cabal/packages/hackage.haskell.org/00-index.cache") home

home :: IO FilePath
home = getHomeDirectory
