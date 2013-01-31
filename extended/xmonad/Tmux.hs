module Tmux where

import Control.Applicative
import Data.List (union)

import           Data.Map (Map, (!))
import qualified Data.Map as M
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
  ss <- io run
  let as = union ss (M.keys db)
  inputPromptWithCompl c "tmux" (mkComplFunFromList' as) ?+ start db ss


start :: Sessions -> [String] -> String -> X ()
start db ss s = do
  term <- asks $ terminal . config
  spawn $ case undefined of
    _ | s `elem` ss     -> attach  term s
      | s `M.member` db -> create' term s (db ! s)
      | otherwise       -> create  term s
 where
  attach t s    = t ++ " -e tmux attach -t " ++ s
  create t s    = t ++ " -e tmux new -s "    ++ s
  create' t s (ChangeDirectory p) =
    t ++ " -e sh -c \"cd " ++ p ++ "; tmux new -s "    ++ s ++ "\""
  create' t s (Session c) =
    t ++ " -e tmux new -s "    ++ s ++ " '" ++ c ++ "'"
