{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -W #-}
-- | Simple routing
module RouteT
  ( -- * RouteT monad
    RouteT, Route, runRouteT, runRoute
    -- * Routing
  , nomore, sofar, nofar, next, rest, dir, dirs
  ) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Category ((>>>))
import Control.Lens
import Control.Monad (MonadPlus(..), ap, guard, liftM)
import Control.Monad.Reader (ReaderT, runReaderT, local)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Identity (Identity(..))
import System.FilePath (joinPath, makeRelative, splitDirectories)
import System.FilePath.Lens ((</>~))

{-# ANN module "Hlint: Unused LANGUAGE pragma" #-}


-- * RouteT definition

type Route = RouteT Identity

newtype RouteT m a = RouteT { unRouteT :: ReaderT Routing (OptionT m) a }
  deriving (Functor)

instance Monad m => Applicative (RouteT m) where
  pure = RouteT . pure
  RouteT f <*> RouteT x = RouteT (f <*> x)

instance Monad m => Monad (RouteT m) where
  return = pure
  RouteT m >>= k = RouteT $ m >>= unRouteT . k

instance Monad m => Alternative (RouteT m) where
  empty = RouteT empty
  RouteT a <|> RouteT b = RouteT (a <|> b)

instance Monad m => MonadPlus (RouteT m) where
  mzero = empty
  mplus = (<|>)

instance MonadIO m => MonadIO (RouteT m) where
  liftIO = RouteT . liftIO

data Routing = Routing
  { _route :: [FilePath]
  , _routed :: FilePath
  }

route :: Lens' Routing [FilePath]
route f r = f (_route r) <&> \x -> r { _route = x }

routed :: Lens' Routing FilePath
routed f r = f (_routed r) <&> \x -> r { _routed = x }

mkRouting :: FilePath -> Routing
mkRouting path = Routing { _route = splitDirectories (makeRelative "/" path), _routed = [] }


-- | Running 'RouteT' gives user either routing failure
-- or some useful value wrapped in underlying monad
runRouteT :: FilePath -> RouteT m a -> m (Maybe a)
runRouteT path = runOptionT . flip runReaderT (mkRouting path) . unRouteT

-- | Running 'RouteT' over 'Identity' gives user either
-- routing failure or some useful value
runRoute :: FilePath -> Route a -> Maybe a
runRoute path = runIdentity . runRouteT path


-- * Routing

-- | Ensure this is the end of the route
nomore :: Monad m => RouteT m ()
nomore = RouteT $ do
  ps <- view route
  guard (null ps)

-- | Get already routed part of the route
sofar :: Monad m => RouteT m FilePath
sofar = RouteT $ view routed

-- | Get already routed path and ensure it's the end of the route
nofar :: Monad m => RouteT m FilePath
nofar = nomore >> sofar

-- | Make routing decision based on next directory
next :: Monad m => (FilePath -> RouteT m b) -> RouteT m b
next k = RouteT $ do
  ps <- view route
  case ps of
    []   -> empty
    d:ds -> local (routed </>~ d >>> route .~ ds) (unRouteT (k d))

-- | Make routing decision based on the rest of the route
rest :: Monad m => ([FilePath] -> RouteT m b) -> RouteT m b
rest k = RouteT $ do
  ps <- view route
  local (routed </>~ joinPath ps >>> route .~ []) (unRouteT (k ps))

-- | One directory deeper
dir :: Monad m => FilePath -> RouteT m a -> RouteT m a
dir path (RouteT r) = RouteT $ do
  ps <- view route
  case ps of
    [] -> empty
    d:ds
      | d == path -> local (routed </>~ d >>> route .~ ds) r
      | otherwise -> empty

-- | Some directories deeper
dirs :: Monad m => FilePath -> RouteT m a -> RouteT m a
dirs path r =
  let directories = splitDirectories (makeRelative "/" path)
  in foldr dir r directories


-- | Sorry, but Control.Monad.Trans.Maybe is unusable
newtype OptionT m a = OptionT { runOptionT :: m (Maybe a) }

instance Monad m => Functor (OptionT m) where
  fmap f (OptionT m) = OptionT (liftM (fmap f) m)

instance Monad m => Applicative (OptionT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (OptionT m) where
  return = OptionT . return . Just
  OptionT x >>= f = OptionT $ do
    v <- x
    case v of
      Nothing -> return Nothing
      Just y  -> runOptionT (f y)

instance Monad m => Alternative (OptionT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (OptionT m) where
  mzero = OptionT (return Nothing)
  OptionT x `mplus` OptionT y = OptionT $ do
    v <- x
    case v of
      Nothing -> y
      Just _  -> return v

instance MonadTrans OptionT where
  lift = OptionT . liftM Just

instance MonadIO m => MonadIO (OptionT m) where
  liftIO = lift . liftIO
