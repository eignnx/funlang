module Cata
  ( Algebra
  , Fix(..)
  , unfix
  , cata
  , RecTyped(..)
  , unRecTyped
  , cataRecTyped
  )
where

import qualified Ty

type Algebra f a = f a -> a

data Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

data RecTyped f = (f (RecTyped f)) :<: Ty.Ty

unRecTyped :: RecTyped f -> f (RecTyped f)
unRecTyped (f :<: _) = f

cataRecTyped :: Functor f => Algebra f a -> RecTyped f -> a
cataRecTyped alg = alg . fmap (cataRecTyped alg) . unRecTyped