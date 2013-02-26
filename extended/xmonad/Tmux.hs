module Tmux where

import Control.Applicative
import Data.List (union)
import Data.Monoid
import System.IO.Error (catchIOError)

import           Data.Map (Map, (!))
import qualified Data.Map as M
import           System.FilePath ((</>))
import           System.Directory (getDirectoryContents, getHomeDirectory)

import           XMonad
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Util.Run


type Sessions = Map String Command

data Command =
    ChangeDirectory FilePath
  | Session String
    deriving (Show, Read, Eq, Ord)


run :: IO [String]
run = lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_name}"] ""


prompt :: Sessions -> XPConfig -> X ()
prompt db c = do
  rs <- io run
  gs <- io gits
  ss <- io svns
  let as = foldr union [] $ rs : map M.keys [gs, ss, db]
  inputPromptWithCompl c "tmux" (mkComplFunFromList' as) ?+ start (mconcat [gs, ss, db]) rs


gits, svns :: IO Sessions
gits = vcs "git"
svns = vcs "svn"


vcs :: FilePath -> IO Sessions
vcs p = catchIOError (repos p) (\_ -> return mempty)


repos :: FilePath -> IO Sessions
repos p = do
  t <- (</> p) <$> getHomeDirectory
  xs <- filter (`notElem` [".", ".."]) <$> getDirectoryContents t
  return . M.fromList . zip xs $ map (ChangeDirectory . (t </>)) xs


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
