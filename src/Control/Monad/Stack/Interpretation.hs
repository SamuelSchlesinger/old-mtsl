module Control.Monad.Stack.Interpretation where

import Control.Monad.Stack
import Control.Monad.Stack.Trans
import Control.Monad.Stack.Constraint

type Interpretation ts m m' = forall x. Stack ts m x -> m' x

(->-) :: (MonadTrans (Stack ts), ForAll Monad '[m, m', m'']) => Interpretation ts m m' -> Interpretation ts m' m'' -> Interpretation ts m m''
i ->- j = j . lift . i

interpret :: Interpretation ts m m' -> Stack ts m a -> m' a
interpret i = (i $)
