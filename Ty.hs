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
  )
where

import           Utils     ( (+++), code, codeIdent )
import qualified Data.Map  as M
import           Data.List ( intercalate )
import Control.Monad (foldM)
import Control.Monad.State (StateT)

data Ty
  = ValTy String
  | VrntTy (M.Map String Ty)
  | TupleTy [Ty]
  | FnTy [Ty] Ty
  | ModTy (M.Map String Ty) -- The type of a module
  deriving Eq

instance Show Ty where
  show (ValTy name) = name
  show (FnTy params ret) = show params +++ "->" +++ show ret
  show (ModTy m) = "{" +++ intercalate ", " ((\(name, ty) -> name ++ ":" +++ show ty) <$> M.toList m) +++ "}"

pattern NeverTy = ValTy "Never"
pattern VoidTy  = ValTy "Void"
pattern BoolTy  = ValTy "Bool"
pattern IntTy   = ValTy "Int"
pattern TextTy  = ValTy "Text"
