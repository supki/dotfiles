{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Semi-usable tmux sessions prompt for XMonad
--
-- Inspired by Simon Gomizelj (@vodik) - https://github.com/vodik/dotfiles/blob/master/xmonad/lib/XMonad/Util/Tmux.hs
module Tmux where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.Array ((!), array, listArray)
import           Data.Foldable (asum, traverse_)
import           Data.Function (on)
import           Data.Ord (comparing)
import           Data.List (intercalate, isPrefixOf, nubBy, sortBy, delete)
import           Data.Maybe (maybeToList)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Map (Map)
import qualified System.Directory as D
import           System.FilePath ((</>))
import           Text.Printf (printf)
import           XMonad hiding (spawn)
import           XMonad.Prompt
import qualified XMonad.StackSet as W
import           XMonad.Util.Run
import           XMonad.Util.NamedWindows (getName)

import           Damit
import           Spawn (spawn)


-- | Ask what session user wants to create/attach to
prompt
  :: [FilePath]               -- ^ Candidate directoroes
  -> XPConfig                 -- ^ Prompt theme
  -> X ()
prompt dirs xpConfig = do
  cs <- active
  ds <- concatMapM ls dirs
  let as = nubBy ((==) `on` un) $ map ('\'' :) cs ++ ds
  mkXPromptWithReturn (UnfuckedInputPrompt "tmux") xpConfig (compl' as) return
    >>= traverse_ (start cs)

newtype UnfuckedInputPrompt = UnfuckedInputPrompt String

instance XPrompt UnfuckedInputPrompt  where
  showXPrompt (UnfuckedInputPrompt s) = s ++ ": "
  commandToComplete _ = id
  nextCompletion _ = getNextCompletion

type PromptKeymap = Map (KeyMask, KeySym) (XP ())

-- | Get currently active tmux sessions' names, except \"scratchpad\" that is
active :: X [String]
active = io $ delete "scratchpad" . lines <$> runProcessWithInput "tmux" ["-S", "/tmp/tmux-1000/default", "list-sessions", "-F", "#{session_name}"] ""

-- | Semifuzzy completion function
compl' :: [String] -> ComplFunction
compl' xs s  = return . sortBy status . sortOn (levenshtein s) . filter (\x -> s `isSubsequenceOf` un x) $ xs
 where
  status ('\'' : _) ('\'' : _) = EQ
  status ('\'' : _) _          = LT
  status _          ('\'' : _) = GT
  status _          _          = EQ

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

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map fst . sortBy (comparing snd) . map (\x -> (x, f x))

levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein xs ys = arr ! (max_i, max_j)
 where
  axs   = listArray (1, max_i) xs
  ays   = listArray (1, max_j) ys
  arr   = array ((0, 0), (max_i, max_j)) [((i, j), go i j) | i <- [0 .. max_i], j <- [0 .. max_j]]
  max_i = length xs
  max_j = length ys
  go i 0 = i
  go 0 j = j
  go i j = minimum [1 + arr ! (i - 1, j), 1 + arr ! (i, j - 1), c + arr ! (i - 1, j - 1)]
   where
    c = if axs ! i == ays ! j then 0 else 2

ls :: String -> X [FilePath]
ls p = io $ do
  map (\x -> p ++ " " ++ x) . filter (`notElem` [".", ".."]) <$> D.getDirectoryContents p
 `mplus`
  return []


-- | Start tmux session terminal
-- May either start a new tmux session if it does not exist or connect to existing session
start :: [String] -> String -> X ()
start runningSessions (un -> userInput) = do
  term <- asks (terminal . config)
  if userInput `elem` runningSessions
    then do
      let nameWindows = mapM (\w -> fmap (\n -> (show n, w)) (getName w)) . W.integrate' . W.stack
      ws <- gets windowset
      kv <- concatMapM nameWindows (W.workspaces ws)
      maybe (create term) (windows . W.focusWindow) (lookup userInput kv)
    else
      create term
 where
  create t = safeSpawn t ("-e" : "damit" : words userInput)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f
