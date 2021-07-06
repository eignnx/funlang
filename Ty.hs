module Ty
  ( Ty(..)
  , (<:)
  , unitTy
  , boolTy
  , intTy
  , textTy
  )
where

import qualified Data.Map as M

data Ty
  = ValTy String
  | FnTy [Ty] Ty
  | ModTy (M.Map String Ty) -- The type of a module
  deriving (Show, Eq)

(<:) :: Ty -> Ty -> Bool
never <: t2 | never == neverTy = True
t1 <: t2 | t1 == t2 = True
-- FnTy x1 y1 <: FnTy x2 y2 = x2 <: x1 && y1 <: y2
ModTy m1 <: ModTy m2 = m2 `M.isSubmapOf` m1
_ <: _ = False

-- TODO: use LANGUAGE PatternSynonyms: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html
neverTy = ValTy "Never"
unitTy  = ValTy "Unit"
boolTy  = ValTy "Bool"
intTy   = ValTy "Int"
textTy  = ValTy "Text"
