{-# LANGUAGE DeriveFunctor #-}
-- | Simple routing
module RouteT
  ( -- * RouteT monad
    RouteT, Route, runRouteT
    -- * Routing
  , nomore, sofar, next, rest, dir, dirs
  ) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Category ((>>>))
import Control.Lens
import Control.Monad (MonadPlus(..), guard)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Trans (MonadIO(..))
import Data.Functor.Identity (Identity)
import Data.Monoid (Monoid(..))
import System.FilePath (joinPath, makeRelative, splitDirectories)
import System.FilePath.Lens ((</>~))


-- * RouteT definition

type Route = RouteT ()

newtype RouteT e m a = RouteT { unRouteT :: ReaderT Routing (EitherT e m) a }
    deriving (Functor)

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


-- | Running 'RouteT' gives user either routing failure or some useful value
runRouteT :: FilePath -> RouteT e m a -> m (Either e a)
runRouteT path = runEitherT . flip runReaderT (mkRouting path) . unRouteT


-- * Routing

-- | Ensure this is the end of the route
nomore :: (Monad m, Monoid e) => RouteT e m ()
nomore = RouteT $ do
  ps <- view route
  guard (null ps)

-- | Get already routed part of the route
sofar :: (Monad m, Monoid e) => RouteT e m FilePath
sofar = RouteT $ view routed

-- | Make routing decision based on next directory
next :: (Monad m, Monoid e) => (FilePath -> RouteT e m b) -> RouteT e m b
next k = RouteT $ do
  ps <- view route
  case ps of
    []   -> empty
    d:ds -> local (routed </>~ d >>> route .~ ds) (unRouteT (k d))

-- | Make routing decision based on the rest of the route
rest :: (Monad m, Monoid e) => ([FilePath] -> RouteT e m b) -> RouteT e m b
rest k = RouteT $ do
  ps <- view route
  local (routed </>~ joinPath ps >>> route .~ []) (unRouteT (k ps))

-- | One directory deeper
dir :: (Monad m, Monoid e) => FilePath -> RouteT e m a -> RouteT e m a
dir path (RouteT r) = RouteT $ do
  ps <- view route
  case ps of
    [] -> empty
    d:ds
      | d == path -> local (routed </>~ d >>> route .~ ds) r
      | otherwise -> empty

-- | Some directories deeper
dirs :: (Monad m, Monoid e) => FilePath -> RouteT e m a -> RouteT e m a
dirs path r =
  let directories = splitDirectories (makeRelative "/" path)
  in foldr dir r directories
