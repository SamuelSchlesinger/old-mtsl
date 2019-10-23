module Control.Monad.Stack.Logic where

newtype LogicT m a = LogicT { runLogicT :: (a -> m r -> m r) -> m r -> m r }


