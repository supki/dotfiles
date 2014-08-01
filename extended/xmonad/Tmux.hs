{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -W #-}
-- | Semi-usable tmux sessions prompt for XMonad
--
-- Inspired by Simon Gomizelj (@vodik) - https://github.com/vodik/dotfiles/blob/master/xmonad/lib/XMonad/Util/Tmux.hs
module Tmux where

import           Control.Applicative
import           Control.Monad
import           Data.Array ((!), array, listArray)
import           Data.Foldable (asum)
import           Data.Function (on)
import           Data.Ord (comparing)
import           Data.List (isPrefixOf, nubBy, sortBy)
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as M
import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.Wordexp.Simple (wordexp)
import           Text.Printf (printf)
import           XMonad hiding (spawn)
import           XMonad.Prompt
import qualified XMonad.StackSet as W
import           XMonad.Prompt.Input
import           XMonad.Util.Run
import           XMonad.Util.NamedWindows (getName)

import           RouteT
import           Spawn (spawn)

{-# ANN routes "HLint: Reduce duplication" #-}


-- | Possible startup commands for tmux sessions
data Command =
    ChangeDirectory FilePath -- ^ Change directory before starting session
  | Session String           -- ^ Start session for specific command
  | Remote String String     -- ^ Start session remotely
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
  let as = nubBy ((==) `on` un) $ map ('\'' :) cs ++ ds
  inputPromptWithCompl
    xpConfig { promptKeymap = promptCustomKeymap <> promptKeymap xpConfig }
    "tmux" (compl' as) ?+ start cs route

type PromptKeymap = Map (KeyMask, KeySym) (XP ())

promptCustomKeymap :: PromptKeymap
promptCustomKeymap = M.singleton (controlMask, xK_j) (do setSuccess True; setDone True)

-- | Get current active tmux sessions names
currents :: X [String]
currents = io $ lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_name}"] ""

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
    then do
      let nameWindows = mapM (\w -> fmap (\n -> (show n, w)) (getName w)) . W.integrate' . W.stack
      ws <- gets windowset
      kv <- concatMapM nameWindows (W.workspaces ws)
      case lookup userInput kv of
        Nothing -> spawn $ createOrAttach term userInput
        Just w  -> windows (W.focusWindow w)
    else do
      routed <- io $ runRouteT userInput route
      case routed of
        Just command -> spawn $ create term userInput command
        Nothing      -> spawn $ createOrAttach term userInput
 where
  createOrAttach t e =
    printf "%s -e tmux new-session -AD -s '%s'" t e
  create t e (ChangeDirectory p) =
    printf "%s -e tmux new-session -c \"${PWD}/%s\" -s '%s'" t p e
  create t e (Session c) =
    printf "%s -e tmux new-session -s '%s' '%s'" t e c
  create t e (Remote s n) =
    printf "%s -e ssh %s -t tmux new-session -AD -s '%s'" t s n

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

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
  , next $ \part -> do
      nomore
      guard ("slave" `isPrefixOf` part)
      return (Tmux.Session ("ssh " ++ part))
  , dir "ko" $
      next $ \name -> do
        nomore
        return (Tmux.Remote "kolyskovi" name)
  , next $ \part -> do
      nomore
      guard (part `elem` ["dev", "storage", "budueba", "jenkins", "victim"])
      return (Tmux.Session ("ssh " ++ part))
  ]

exists :: FilePath -> RouteT IO ()
exists path = do
  p <- io (D.doesDirectoryExist path)
  guard p
