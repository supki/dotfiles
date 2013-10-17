{-# OPTIONS_GHC -W #-}
module Man where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input

prompt :: XPConfig -> X ()
prompt c = inputPrompt c "man" ?+ man
 where
  man s = spawn $ unwords ["urxvt", "-e", "/usr/bin/zsh", "-i", "-c", "'man " ++ s ++ "'"]
