module Control.Monad.Stack.Adjoint where

import Data.Functor.Identity
import Data.Functor.Compose

-- The laws that this must satisfy are
-- fmap counit . unit = id
-- counit . fmap unit = id
-- or...
-- leftAdj counit = id
-- rightAdj unit  = it
class (Functor f, Functor g) => Adjunction f g where
  unit :: a -> f (g a)
  counit :: g (f a) -> a
  leftAdj :: (g a -> b) -> a -> f b
  rightAdj :: (a -> f b) -> g a -> b
  leftAdj f = fmap f . unit
  rightAdj f = counit . fmap f

-- Proof of the laws: 
-- Here, we can prove the first law with the help of GHC.
-- :t fmap (uncurry (flip ($))) . flip (,)
--   fmap (uncurry (flip ($))) . flip (,) :: forall a b. (a -> b) -> a -> b
-- The only function with this type is the identity
--
-- Lets try it for the second law.
-- :t uncurry (flip ($)) . fmap (flip (,))
--   uncurry (flip ($)) . fmap (flip (,)) :: (a, b) -> (a, b)
-- The same as above!
instance Adjunction ((->) b) ((,) b) where
  unit = flip (,)
  counit = uncurry (flip ($))

-- Proof of the laws can be seen through GHCi yet again, left as an
-- exercise to the reader.
instance Adjunction Identity Identity where
  unit = Identity . Identity
  counit = runIdentity . runIdentity

-- TODO Here, we have to do a little more work...
instance (Adjunction f g, Adjunction f' g') => Adjunction (Compose f' f) (Compose g g') where
  unit = Compose . leftAdj (leftAdj Compose)
  counit = rightAdj (rightAdj getCompose) . getCompose

-- TODO Implement the adjunction between Free and CoFree
