module Tmux where

import Control.Applicative

import           XMonad
import           XMonad.Prompt
import           XMonad.Prompt.Input
import           XMonad.Util.Run


run :: IO [String]
run = lines <$> runProcessWithInput "tmux" ["list-sessions", "-F", "#{session_name}"] ""


prompt :: XPConfig -> X ()
prompt c = io run >>= \as -> inputPromptWithCompl c "tmux" (mkComplFunFromList' as) ?+ start as


start :: [String] -> String -> X ()
start ss s = asks (terminal . config) >>= \term -> if s `elem` ss then attach term s else create term s
 where
  attach = \t s -> spawn $ t ++ " -e tmux attach -t " ++ s
  create = \t s -> spawn $ t ++ " -e tmux new -s "    ++ s
