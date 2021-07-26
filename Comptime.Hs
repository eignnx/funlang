{-# LANGUAGE LambdaCase #-}

module Comptime
  ( eval
  )
where

import qualified Ast
import           Ast ( RecTyped((:<:)) )
import qualified Hir

eval :: Ast.TypedExpr -> Hir.Value
eval = \case
  Ast.LiteralF (Ast.Bool b) :<: _   -> Hir.VBool b
  Ast.LiteralF (Ast.Int i) :<: _    -> Hir.VInt $ fromIntegral i
  Ast.LiteralF (Ast.String t) :<: _ -> Hir.VString t