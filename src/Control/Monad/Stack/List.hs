module Control.Monad.Stack.List where

import Control.Monad.Stack.N
import GHC.Types (Constraint)

type Index (t :: k) (ts :: [k]) = IndexGo t ts 'Z
type family IndexGo (t :: k) (ts :: [k]) (n :: N) where
  IndexGo t (t ': ts) n = n
  IndexGo t (x ': ts) n = IndexGo t ts (S n)

type Prior t1 t2 ts = Compare (Index t1 ts) (Index t2 ts) 'LT

type family Ordered (ms :: [k]) (ts :: [k]) :: Constraint where
  Ordered (x ': y ': ms) ts = (Prior x y ts, Ordered (y ': ms) ts)
  Ordered (x ': '[])     ts = ()
  Ordered '[]            ts = ()
