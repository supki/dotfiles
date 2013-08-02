module Startup
  ( myStartupHook
  ) where

import XMonad

import qualified Profile

myStartupHook :: X ()
myStartupHook =
  mapM_ spawnOnce Profile.apps
 where
  spawnOnce (target, command) = spawn $ "pid=\"$( pgrep -f \"^" ++ target ++ "\")\"; [ -z \"${pid}\" ] && " ++ command
