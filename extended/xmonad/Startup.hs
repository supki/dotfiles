module Startup (myStartupHook) where

import XMonad

import qualified Profile         as P

myStartupHook :: X ()
myStartupHook = mapM_ spawnOnce P.apps
  where
    spawnOnce :: (MonadIO m) => (String, String) -> m ()
    spawnOnce (target, command) = spawn $ "pid=\"$( pgrep -f \"^" ++ target ++ "\")\"; [ -z \"${pid}\" ] && " ++ command

