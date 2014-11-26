{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Route
  ( RouteT
  , run
  , Route
  , Env
  , Match
  , Input
  , input
  , route
  , Param(..)
  , arg
  , args
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Function (on)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.List (stripPrefix)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe)
import           Data.String (IsString(..))
import           Data.Traversable (traverse)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Text.Read (readMaybe)


type Input = String

newtype Route = Route String

instance IsString Route where
  fromString = Route . fromString

type Match = Map String [String]

type Env = (Match, Input)


newtype RouteT m a = RouteT { unRouteT :: Env -> m (Maybe a)
  } deriving (Functor)

instance Applicative m => Applicative (RouteT m) where
  pure = RouteT . const . pure . Just
  RouteT f <*> RouteT g = RouteT ((liftA2.liftA2) (<*>) f g)

instance Applicative m => Alternative (RouteT m) where
  empty = RouteT (const (pure Nothing))
  RouteT f <|> RouteT g = RouteT ((liftA2.liftA2) (<|>) f g)

instance Monad m => Monad (RouteT m) where
  return = RouteT . const . return . Just
  RouteT mx >>= k = RouteT $ \e -> maybe (return Nothing) (\a -> unRouteT (k a) e) =<< mx e

instance Monad m => MonadPlus (RouteT m) where
  mzero = RouteT (const (return Nothing))
  RouteT f `mplus` RouteT g = RouteT ((liftM2.liftM2) mplus f g)

instance MonadIO m => MonadIO (RouteT m) where
  liftIO = RouteT . pure . liftM Just . liftIO

run ::  RouteT m a -> Input -> m (Maybe a)
run (RouteT f) i = f (Map.empty, i)

route :: Monad m => Route -> RouteT m a -> RouteT m a
route r (RouteT f) = RouteT $ \(_, i) -> maybe (return Nothing) (\m -> f (m, i)) (match r i)

match :: Route -> Input -> Maybe Match
match (Route r) i = do
  ps <- (zip' `on` words) i r
  Map.fromListWith (flip (++)) . concat <$> traverse bind ps
 where
  bind (x, y)
    | x == y = Just []
    | Just k <- stripPrefix "$" y
             = Just [(k, [x])]
    | otherwise
             = Nothing

zip' :: [a] -> [b] -> Maybe [(a, b)]
zip' []       []       = Just []
zip' (x : xs) (y : ys) = fmap ((x, y) :) (zip' xs ys)
zip' _        _        = Nothing


class Param a where
  parse :: String -> Maybe a
  default parse :: Read a => String -> Maybe a
  parse = readMaybe

instance Param String where
  parse = Just

instance Param Integer
instance Param Int
instance Param Int64
instance Param Int32
instance Param Int16
instance Param Int8
instance Param Word
instance Param Word64
instance Param Word32
instance Param Word16
instance Param Word8

instance Param Double
instance Param Float

arg :: (Monad m, Param a) => String -> RouteT m a
arg k = RouteT (return . (parse <=< listToMaybe <=< Map.lookup k . fst))

args :: (Monad m, Param a) => String -> RouteT m [a]
args k = RouteT (return . (traverse parse <=< Map.lookup k . fst))

input :: Monad m => RouteT m String
input = RouteT $ \(_, i) -> return (Just i)
