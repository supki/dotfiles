-- | Semi-usable tmux sessions prompt for XMonad
module Tmux where

import Control.Applicative
import Control.Monad
import Data.List (union)
import Data.Monoid

import           Data.Map (Map, (!))
import qualified Data.Map as M
import           System.FilePath ((</>))
import           System.Directory (doesDirectoryExist, getDirectoryContents, getHomeDirectory)

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.Run


-- | Less typing type synonym
type Sessions = Map String Command

-- | Possible startup commands for tmux sessions
data Command =
    ChangeDirectory FilePath -- ^ Change directory before starting session
  | Session String           -- ^ Start session for specific command
    deriving (Show, Read, Eq, Ord)


-- | Get current active tmux sessions names
currents :: X [String]
currents = io $ lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_name}"] ""


-- | Ask what session user wants to create/attach to
prompt :: Sessions   -- ^ Default user defined sessions
       -> [FilePath] -- ^ Roots for ChangeDirectory sessions
       -> XPConfig   -- ^ Prompt theme
       -> X ()
prompt db roots c = do
  cs <- currents
  ss <- mapM under roots
  let as = foldr union [] $ cs : map M.keys (ss ++ [db])
  inputPromptWithCompl c "tmux" (mkComplFunFromList' as) ?+ start (mconcat (ss ++ [db])) cs


-- | List directories under root as ChangeDirectory sessions
under :: FilePath   -- ^ Directory root
      -> X Sessions -- ^ Sessions for each directory under the root
under p = io $ directories `mplus` return mempty
 where
  directories = do
    t <- (</> p) <$> getHomeDirectory
    xs <- filter (`notElem` [".", ".."]) <$> getDirectoryContents t
    xs <- filterM (\d -> doesDirectoryExist $ t </> d) xs
    return . M.fromList . zip xs $ map (ChangeDirectory . (t </>)) xs


-- | Start tmux session terminal
-- May either start a new tmux session if it does not exist or connect to existing session
start :: Sessions -> [String] -> String -> X ()
start as ss s = do
  term <- asks $ terminal . config
  spawn $ case undefined of
    _ | s `elem` ss     -> attach  term s
      | s `M.member` as -> create' term s (as ! s)
      | otherwise       -> create  term s
 where
  attach t s = t ++ " -e tmux attach -t " ++ s
  create t s = t ++ " -e tmux new -s "    ++ s
  create' t s (ChangeDirectory p) =
    t ++ " -e sh -c \"cd " ++ p ++ "; tmux new -s "    ++ s ++ "\""
  create' t s (Session c) =
    t ++ " -e tmux new -s "    ++ s ++ " '" ++ c ++ "'"
