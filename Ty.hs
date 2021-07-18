{-# LANGUAGE PatternSynonyms #-}

module Ty
  ( Ty( ..
      , NeverTy
      , VoidTy
      , BoolTy
      , IntTy
      , TextTy
      )
  , (<:)
  , (-&&>)
  )
where

import qualified Data.Map as M
import           Data.List ( intercalate )

data Ty
  = ValTy String
  | FnTy [Ty] Ty
  | ModTy (M.Map String Ty) -- The type of a module
  deriving Eq

instance Show Ty where
  show (ValTy name) = name
  show (FnTy params ret) = show params ++ " -> " ++ show ret
  show (ModTy m) = "{ " ++ intercalate ", " ((\(name, ty) -> name ++ ": " ++ show ty) <$> M.toList m) ++ " }"

(<:) :: Ty -> Ty -> Bool
NeverTy <: t2 = True
t1 <: t2 | t1 == t2 = True
FnTy x1 y1 <: FnTy x2 y2 = all (\(x1', x2') -> x2' <: x1') (zip x1 x2) && y1 <: y2
ModTy m1 <: ModTy m2 = m2 `M.isSubmapOf` m1
_ <: _ = False

pattern NeverTy = ValTy "Never"
pattern VoidTy  = ValTy "Void"
pattern BoolTy  = ValTy "Bool"
pattern IntTy   = ValTy "Int"
pattern TextTy  = ValTy "Text"

-- | Short-circuits a type if the first type is `Never`.
--   For instance, in the block expression `do intr.exit[]; 1 end`, the entire
--   block ought to have type `Never -&&> Int` since `intr.exit[]` has type
--   `Never`, and it appears BEFORE the expression `1`. Therefore, the entire
--   block should have type `Never`.
(-&&>) :: Ty -> Ty -> Ty
NeverTy -&&> _ = NeverTy
_ -&&> ty = ty
