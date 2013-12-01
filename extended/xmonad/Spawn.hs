module Spawn (spawn) where

import           Codec.Binary.UTF8.String (encodeString)
import           Control.Monad (liftM)
import           Control.Monad.Trans (MonadIO)
import           System.Posix.Process (executeFile)
import qualified XMonad as X


spawn :: MonadIO m => String -> m ()
spawn x = void . X.xfork $ executeFile "/bin/sh" False ["-l", "-c", encodeString x] Nothing
 where
  void = liftM (const ())
