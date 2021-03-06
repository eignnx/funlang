module Res
  ( Res (..),
    Error (..),
    toRes,
    addError,
    ensureM,
    unwrapRes
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Trans (MonadTrans (lift))
import Utils (code, indent, (+++))

data Res a
  = Ok a
  | Err Error

instance Show a => Show (Res a) where
  show (Ok a) = "Ok:" +++ code a
  show (Err e) = "Err:" +++ code e

instance Semigroup a => Semigroup (Res a) where
  (Ok a) <> (Ok b) = Ok (a <> b)
  (Err a) <> (Err b) = Err (a <> b)
  (Ok a) <> (Err b) = Err b
  (Err a) <> (Ok b) = Err a

instance Monoid a => Monoid (Res a) where
  mempty = Ok mempty

instance Functor Res where
  fmap f (Ok a) = Ok (f a)
  fmap f (Err e) = Err e

instance Applicative Res where
  pure x = Ok x
  liftA2 f (Ok x) (Ok y) = Ok (f x y)
  liftA2 f (Err e1) (Err e2) = Err (e1 <> e2)
  liftA2 f (Err e) (Ok x) = Err e
  liftA2 f (Ok x) (Err e) = Err e

instance Monad Res where
  (Ok x) >>= f = f x
  (Err e) >>= f = Err e

instance MonadFail Res where
  fail msg = Err $ RootCause msg

data Error
  = RootCause String
  | ResultingError String Error
  | MultiError Error Error

instance Show Error where
  show (RootCause explanation) = explanation ++ "."
  show (ResultingError extraInfo e) = extraInfo +++ "because..." ++ indent (show e)
  show (MultiError e1 e2) = show e1 ++ "\n\nAlso...\n\n" ++ show e2

instance Semigroup Error where
  e1 <> e2 = MultiError e1 e2

toRes :: Maybe a -> Error -> Res a
toRes (Just a) _ = Ok a
toRes Nothing reason = Err reason

addError :: Res a -> String -> Res a
addError (Ok a) _ = Ok a
addError (Err err) errMsg = Err (ResultingError errMsg err)

ensureM :: (Monad m, MonadFail m) => m Bool -> String -> m a -> m a
ensureM cond failureMsg program = do
  bool <- cond
  if bool
    then program
    else fail failureMsg

unwrapRes (Ok a) = a
unwrapRes (Err e) = error $ show e
