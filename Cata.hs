module Cata
  ( Algebra
  , Fix(..)
  , unfix
  , cata
  , andThen
  )
where

type Algebra f a = f a -> a

data Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

andThen :: Algebra f (Fix f) -> Algebra f (Fix f) -> Algebra f (Fix f)
f `andThen` g = g . unfix . f