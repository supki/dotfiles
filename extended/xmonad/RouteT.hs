{-# LANGUAGE DeriveFunctor #-}
module RouteT where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad (MonadPlus(..), guard)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Trans (MonadIO(..))
import Data.Functor.Identity (Identity)
import Data.Monoid (Monoid(..))
import System.FilePath (makeRelative, splitDirectories)


-- * RouteT definition

type Route = RouteT ()

newtype RouteT e m a = RouteT { unRouteT :: ReaderT RoutePath (EitherT e m) a }
    deriving (Functor)

type RoutePath = [FilePath]
type RouteError = String

instance Monad m => Applicative (RouteT e m) where
  pure = RouteT . pure
  RouteT f <*> RouteT x = RouteT (f <*> x)

instance Monad m => Monad (RouteT e m) where
  return = pure
  RouteT m >>= k = RouteT $ m >>= unRouteT . k

instance (Monad m, Monoid e) => Alternative (RouteT e m) where
  empty = RouteT empty
  RouteT a <|> RouteT b = RouteT (a <|> b)

instance (Monad m, Monoid e) => MonadPlus (RouteT e m) where
  mzero = empty
  mplus = (<|>)

instance (MonadIO m, Monoid e) => MonadIO (RouteT e m) where
  liftIO = RouteT . liftIO


-- | Running 'RouteT' gives user either routing failure or some useful value
runRouteT :: FilePath -> RouteT e m a -> m (Either e a)
runRouteT path = runEitherT . flip runReaderT (splitDirectories (makeRelative "/" path)) . unRouteT


-- * Routing

-- | One directory deeper
dir :: (Monad m, Monoid e) => FilePath -> RouteT e m a -> RouteT e m a
dir path (RouteT r) = RouteT $ do
  env <- ask
  case env of
    [] -> empty
    d:ds
      | d == path -> local (const ds) r
      | otherwise -> empty

-- | Some directories deeper
dirs :: (Monad m, Monoid e) => FilePath -> RouteT e m a -> RouteT e m a
dirs path route =
  let directories = splitDirectories (makeRelative "/" path)
  in foldr dir route directories

-- | Ensure the end of the route
end :: (Monad m, Monoid e) => RouteT e m ()
end = RouteT $ do
  env <- ask
  guard (null env)

-- | Make routing decision based on next directory
next :: (Monad m, Monoid e) => (FilePath -> RouteT e m b) -> RouteT e m b
next k = RouteT $ do
  env <- ask
  case env of
    []   -> empty
    d:ds -> local (const ds) (unRouteT (k d))

-- | Make routing decision based on the rest of the route
rest :: (Monad m, Monoid e) => ([FilePath] -> RouteT e m b) -> RouteT e m b
rest k = RouteT $ do
  env <- ask
  local (const []) (unRouteT (k env))
