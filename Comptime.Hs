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

  -- Literals are trivially comptime evaluable.
  Ast.LiteralF (Ast.Bool x) :<: _   -> Hir.VBool x
  Ast.LiteralF (Ast.Int x) :<: _    -> Hir.VInt $ fromIntegral x
  Ast.LiteralF (Ast.String x) :<: _ -> Hir.VString x

  Ast.UnaryF Ast.Not e :<: _ -> let
    Hir.VBool x = eval e
    in Hir.VBool $ not x

  Ast.UnaryF Ast.Neg e :<: _ -> let
    Hir.VInt x = eval e
    in Hir.VInt $ negate x

  Ast.BinaryF op x y :<: _ -> case op of
    Ast.ArithOp Ast.Add    -> evalIntIntBinOp (+) x y
    Ast.ArithOp Ast.Sub    -> evalIntIntBinOp (-) x y
    Ast.ArithOp Ast.Mul    -> evalIntIntBinOp (*) x y
    Ast.ArithOp Ast.Div    -> evalIntIntBinOp div x y
    Ast.BoolOp Ast.And     -> evalBoolBoolBinOp (&&) x y
    Ast.BoolOp Ast.Or      -> evalBoolBoolBinOp (||) x y
    Ast.BoolOp Ast.Xor     -> evalBoolBoolBinOp xor x y
    Ast.RelOp Ast.Gt       -> evalIntBoolBinOp (>) x y
    Ast.RelOp Ast.Lt       -> evalIntBoolBinOp (<) x y
    Ast.RelOp Ast.Eq       -> evalValueBoolBinOp (==) x y
    Ast.RelOp Ast.Neq      -> evalValueBoolBinOp (/=) x y
    Ast.OtherOp Ast.Concat -> evalTextTextBinOp (++) x y

  -- Ast.IfF r r r
  -- Ast.AnnF r Ty.Ty

evalIntIntBinOp :: (Int -> Int -> Int) -> Ast.TypedExpr -> Ast.TypedExpr -> Hir.Value
evalIntIntBinOp op x y = let
  Hir.VInt x' = eval x
  Hir.VInt y' = eval y
  in Hir.VInt $ x' `op` y'

evalIntBoolBinOp :: (Int -> Int -> Bool) -> Ast.TypedExpr -> Ast.TypedExpr -> Hir.Value
evalIntBoolBinOp op x y = let
  Hir.VInt x' = eval x
  Hir.VInt y' = eval y
  in Hir.VBool $ x' `op` y'

evalBoolBoolBinOp :: (Bool -> Bool -> Bool) -> Ast.TypedExpr -> Ast.TypedExpr -> Hir.Value
evalBoolBoolBinOp op x y = let
  Hir.VBool x' = eval x
  Hir.VBool y' = eval y
  in Hir.VBool $ x' `op` y'

xor a b = a || b && (not $ a && b)

evalValueBoolBinOp :: (Hir.Value -> Hir.Value -> Bool) -> Ast.TypedExpr -> Ast.TypedExpr -> Hir.Value
evalValueBoolBinOp op x y = let
  x' = eval x
  y' = eval y
  in Hir.VBool $ x' `op` y'

evalTextTextBinOp :: (String -> String -> String) -> Ast.TypedExpr -> Ast.TypedExpr -> Hir.Value
evalTextTextBinOp op x y = let
  Hir.VString x' = eval x
  Hir.VString y' = eval y
  in Hir.VString $ x' `op` y'