module MaybeT where

import Control.Monad.Trans (MonadTrans (..))
import Data.Functor ((<&>))

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

instance Monad m => Functor (MaybeT m) where
  fmap f m = MaybeT $ do
    unwrapped <- runMaybeT m
    return $ fmap f unwrapped

instance Monad m => Applicative (MaybeT m) where
  pure a = MaybeT $ return (Just a)
  f <*> m = MaybeT $ do
    ufunc <- runMaybeT f
    uarg <- runMaybeT m
    return $ ufunc <*> uarg

instance Monad m => Monad (MaybeT m) where
  return a = MaybeT $ return (Just a)
  mvalue >>= f = MaybeT $ do
    unwrapped <- runMaybeT mvalue
    case unwrapped of
      Nothing -> return Nothing
      Just x -> runMaybeT (f x)

instance MonadTrans MaybeT where
  lift m = MaybeT $ m <&> Just
