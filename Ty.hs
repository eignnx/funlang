{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module Ty
  ( Ty( ..
      , NeverTy
      , VoidTy
      , BoolTy
      , IntTy
      , TextTy
      , TupleTy
      )
  , (<:)
  , (-&&>)
  , (>||<)
  )
where

import           Utils     ( (+++), code, codeIdent )
import qualified Data.Map  as M
import           Data.List ( intercalate )

data Ty
  = TyVar String
  | ValTy String [Ty]
  | FnTy [Ty] Ty
  | ModTy (M.Map String Ty) -- The type of a module
  deriving Eq

instance Show Ty where
  show (ValTy name []) = name
  show (ValTy name args) = name ++ "[" ++ intercalate ", " (map show args) ++ "]"
  show (FnTy params ret) = show params +++ "->" +++ show ret
  show (ModTy m) = "{" +++ intercalate ", " ((\(name, ty) -> name ++ ":" +++ show ty) <$> M.toList m) +++ "}"

(<:) :: Ty -> Ty -> Bool
NeverTy <: t2 = True
t1 <: t2 | t1 == t2 = True
ValTy n1 x1 <: ValTy n2 x2 = n1 == n2 && all (uncurry (<:)) (zip x1 x2)
FnTy x1 y1 <: FnTy x2 y2 = all (uncurry (<:)) (zip x1 x2) && y1 <: y2
ModTy m1 <: ModTy m2 = m2 `M.isSubmapOf` m1
_ <: _ = False

pattern NeverTy     = ValTy "Never" []
pattern VoidTy      = ValTy "Void" []
pattern BoolTy      = ValTy "Bool" []
pattern IntTy       = ValTy "Int" []
pattern TextTy      = ValTy "Text" []
pattern TupleTy ts  = ValTy "Tuple" ts

-- | Short-circuits a type if the first type is `Never`.
--   For instance, in the block expression `do intr.exit[]; 1 end`, the entire
--   block ought to have type `Never -&&> Int` since `intr.exit[]` has type
--   `Never`, and it appears BEFORE the expression `1`. Therefore, the entire
--   block should have type `Never`.
(-&&>) :: Ty -> Ty -> Ty
NeverTy -&&> _ = NeverTy
_ -&&> ty = ty

-- | This operator is used to join the types of two branches. It is always the
--   case that `Never >||< ty` or `ty >||< Never` is `ty`. In general, this
--   operator returns the supertype of its two arguments.
--   NOTE: It only works if the two types are related via `<:`.
---  HMMM: Should `Int >||< Text` be `Any`? Probably not.
(>||<) :: Ty -> Ty -> Ty
sub >||< super | sub <: super = super
super >||< sub | sub <: super = super
_ >||< _ = error "Operator `>||<` can only accept types that are related!"
