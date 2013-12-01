{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -W #-}
-- | Semi-usable tmux sessions prompt for XMonad
--
-- Inspired by Simon Gomizelj (@vodik) - https://github.com/vodik/dotfiles/blob/master/xmonad/lib/XMonad/Util/Tmux.hs
module Tmux where

import Control.Applicative
import Control.Monad
import Data.Foldable (asum)
import Data.Function (on)
import Data.List (isPrefixOf, nubBy, sort)

import           Data.Map (Map)
import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.Wordexp.Simple (wordexp)

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.Run

import RouteT


-- | Less typing type synonym
type Sessions = Map String Command

-- | Possible startup commands for tmux sessions
data Command =
    ChangeDirectory FilePath -- ^ Change directory before starting session
  | Session String           -- ^ Start session for specific command
    deriving (Show, Read, Eq, Ord)

-- | Ask what session user wants to create/attach to
prompt
  :: [String]          -- ^ Candidates patterns
  -> RouteT IO Command -- ^ Routing
  -> XPConfig          -- ^ Prompt theme
  -> X ()
prompt patterns route xpConfig = do
  cs <- currents
  ds <- concatMapM expand patterns
  let as = sort . nubBy ((==) `on` un) $ map ('\'' :) cs ++ ds
  inputPromptWithCompl xpConfig "tmux" (compl' as) ?+ start cs route

-- | Get current active tmux sessions names
currents :: X [String]
currents = io $ lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_name}"] ""

-- | Semifuzzy completion function
compl' :: [String] -> ComplFunction
compl' xs s  = return $ filter (\x -> s `isSubsequenceOf` un x) xs

-- | Unquote string if it's quoted
un :: String -> String
un ('\'' : s) = s
un         s  = s

-- | A bit smarter tmux sessions search than 'isSuffixOf'
-- Checks that the first string is a subsequence of the second, that is
-- all its characters are present in the second string in the same order
isSubsequenceOf :: String -> String -> Bool
isSubsequenceOf []     _  = True
isSubsequenceOf (x:xs) ys =
  case dropWhile (/= x) ys of
    _ : zs -> xs `isSubsequenceOf` zs
    []     -> False


-- | Safely expands wordexp pattern catching IO errors
expand :: String -> X [FilePath]
expand p = io $
  wordexp p `mplus` return []


-- | Start tmux session terminal
-- May either start a new tmux session if it does not exist or connect to existing session
start :: [String] -> RouteT IO Command -> String -> X ()
start runningSessions route (un -> userInput) = do
  term <- asks $ terminal . config
  if userInput `elem` runningSessions
    then spawn $ attach  term userInput
    else do
      routed <- io $ runRouteT userInput route
      case routed of
        Just command -> spawn $ create' term userInput command
        Nothing      -> spawn $ create  term userInput
 where
  attach t e = t ++ " -e tmux attach -d -t " ++ e
  create t e = t ++ " -e tmux new -s "    ++ e
  create' t e (ChangeDirectory p) =
    t ++ " -e sh -c \"cd " ++ p ++ "; tmux new -s "    ++ e ++ "\""
  create' t e (Session c) =
    t ++ " -e tmux new -s "    ++ e ++ " '" ++ c ++ "'"

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

{-# ANN routes "HLint: Reduce duplication" #-}

routes :: RouteT IO Command
routes = asum
  [ next $ \repos -> do
      guard (repos `elem` ["git", "svn"])
      next $ \_ -> do
        path <- nofar
        exists path
        return (Tmux.ChangeDirectory path)
  , dir "play" $ asum
      [ next $ \inside -> do
          nomore
          let path = "playground" </> inside
          exists path
          return (Tmux.ChangeDirectory path)
      , return (Tmux.ChangeDirectory "playground")
      ]
  , dirs ".vim/bundle" $
      next $ \_ -> do
        path <- nofar
        exists path
        return (Tmux.ChangeDirectory path)
  , next $ \part -> do
      nomore
      guard ("slave" `isPrefixOf` part)
      return (Tmux.Session ("ssh " ++ part))
  , next $ \part -> do
      nomore
      guard (part `elem` ["dev", "storage", "budueba", "jenkins"])
      return (Tmux.Session ("ssh " ++ part))
  ]

exists :: FilePath -> RouteT IO ()
exists path = do
  p <- io (D.doesDirectoryExist path)
  guard p
