module Control.Monad.Stack.State where

import Control.Monad
import Control.Monad.Stack.Trans

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }

instance Functor f => Functor (StateT s f) where
  fmap f = StateT . fmap (fmap (fmap f)) . runStateT

instance Monad f => Applicative (StateT s f) where
  (<*>) = ap
  pure = return

instance Monad m => Monad (StateT s m) where
  ma >>= f = StateT \s0 -> do
    (s1, a) <- runStateT ma s0
    runStateT (f a) s1

instance MonadTrans (StateT s) where
  lift ma = StateT \s -> do
   a <- ma
   return (s, a)

class Monad m => MonadState s m where
  {-# MINIMAL (get, put) | state #-}
  get :: m s
  put :: s -> m ()
  state :: (s -> (s, a)) -> m a
  get = state \s -> (s, s)
  put s = state \_ -> (s, ())
  state f = do
    (s, a) <- fmap f get
    put s
    return a

instance Monad m => MonadState s (StateT s m) where
  get = StateT \s -> return (s, s)
  put s = StateT \_ -> return (s, ())
  state f = StateT (return . f)

class (forall m. Monad m => MonadState s (t m), MonadTrans t) => MonadStateT s t
instance MonadStateT s (StateT s)

  
