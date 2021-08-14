{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module Ty
  ( Ty( ..
      , NeverTy
      , VoidTy
      , BoolTy
      , IntTy
      , TextTy
      )
    , tySize
    , isZeroSized
  )
where

import           Utils     ( (+++), code, codeIdent, braces, commaSep, list, optList )
import qualified Data.Map  as M
import           Data.List ( intercalate )
import Control.Monad (foldM)
import Control.Monad.State (StateT)

data Ty
  = ValTy String
  | AliasTy String
  | VrntTy (M.Map String [Ty])
  | TupleTy [Ty]
  | FnTy [Ty] Ty
  | ModTy (M.Map String Ty) -- The type of a module
  | RecTy String Ty
  | TyVar String
  deriving (Eq, Ord)

instance Show Ty where
  show (ValTy name) = "$" ++ name
  show (AliasTy name) = name
  show (VrntTy vrnts) = braces $ intercalate " | " (uncurry f <$> M.toList vrnts)
    where f ctorName ctorParams = ctorName +++ commaSep (map show ctorParams)
  show (TupleTy ts) = "Tuple" ++ list ts
  show (FnTy params ret) = list params +++ "->" +++ show ret
  show (ModTy m) = braces $ commaSep (uncurry f <$> M.toList m)
    where f name ty = name ++ ":" +++ show ty
  show (RecTy x body) = "rec" +++ show (TyVar x) +++ "{" +++ show body +++ "}"
  show (TyVar name) = name

pattern NeverTy = ValTy "Never"
pattern VoidTy  = ValTy "Void"
pattern BoolTy  = ValTy "Bool"
pattern IntTy   = ValTy "Int"
pattern TextTy  = ValTy "Text"

tySize :: Ty -> Int
tySize = \case
  NeverTy -> 0 -- Doesn't matter what size it is, it'll never get here.
  VoidTy  -> 0
  _       -> 1

isZeroSized :: Ty -> Bool
isZeroSized = (==0) . tySize
