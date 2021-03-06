module Startup
  ( myStartupHook
  ) where

import           XMonad hiding (spawn)

import qualified Profile
import           Spawn

myStartupHook :: X ()
myStartupHook =
  mapM_ spawnOnce Profile.apps
 where
  spawnOnce (target, command) = spawn $
    "pgrep -f \"^" ++ target ++ "\" || " ++ command
