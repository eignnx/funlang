module Cata
  ( Algebra
  , Unwrap(..)
  , cata
  , Fix(..)
  , RecTyped(..)
  )
where

import qualified Ty

type Algebra f a = f a -> a

class Unwrap y where
  unwrap :: y f -> f (y f)

-- Type variable `y` is something like `Fix` or `RecTyped`.
-- The argument `un` is something like `unfix` or `unRecTyped`.
cata :: (Functor f, Unwrap y) => Algebra f a -> y f -> a
cata alg = alg . fmap (cata alg) . unwrap

data Fix f = Fix (f (Fix f))

instance Unwrap Fix where
  unwrap (Fix f) = f

data RecTyped f = (f (RecTyped f)) :<: Ty.Ty

instance Unwrap RecTyped where
  unwrap (f :<: _) = f
