{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Comptime
  ( eval
  )
where

import qualified Ast
import qualified Hir
import           Utils ( (+++), code )
import           Cata  ( cata, RecTyped(..) )

eval :: Ast.TypedExpr -> Hir.Value
eval = cata \case

  -- Literals are trivially comptime evaluable.
  Ast.LiteralF (Ast.Bool x)   -> Hir.VBool x
  Ast.LiteralF (Ast.Int x)    -> Hir.VInt $ fromIntegral x
  Ast.LiteralF (Ast.Text x)   -> Hir.VText x

  Ast.UnaryF Ast.Not e -> let
    Hir.VBool x = e
    in Hir.VBool $ not x

  Ast.UnaryF Ast.Neg e -> let
    Hir.VInt x = e
    in Hir.VInt $ negate x

  Ast.BinaryF op x y -> case op of
    Ast.ArithOp Ast.Add    -> evalIntIntBinOp (+) x y
    Ast.ArithOp Ast.Sub    -> evalIntIntBinOp (-) x y
    Ast.ArithOp Ast.Mul    -> evalIntIntBinOp (*) x y
    Ast.ArithOp Ast.Div    -> evalIntIntBinOp div x y
    Ast.BoolOp Ast.And     -> evalBoolBoolBinOp (&&) x y
    Ast.BoolOp Ast.Or      -> evalBoolBoolBinOp (||) x y
    Ast.BoolOp Ast.Xor     -> evalBoolBoolBinOp xor x y
    Ast.RelOp Ast.Gt       -> evalIntBoolBinOp (>) x y
    Ast.RelOp Ast.Lt       -> evalIntBoolBinOp (<) x y
    Ast.OtherOp Ast.Concat -> evalTextTextBinOp (++) x y

  -- Ast.IfF r r r
  -- Ast.AnnF r Ty.Ty

  other -> error $ "I don't yet know how to evaluate something at compile time. Sorry!"

evalIntIntBinOp :: (Int -> Int -> Int) -> Hir.Value -> Hir.Value -> Hir.Value
evalIntIntBinOp op x y = let
  Hir.VInt x' = x
  Hir.VInt y' = y
  in Hir.VInt $ x' `op` y'

evalIntBoolBinOp :: (Int -> Int -> Bool) -> Hir.Value -> Hir.Value -> Hir.Value
evalIntBoolBinOp op x y = let
  Hir.VInt x' = x
  Hir.VInt y' = y
  in Hir.VBool $ x' `op` y'

evalBoolBoolBinOp :: (Bool -> Bool -> Bool) -> Hir.Value -> Hir.Value -> Hir.Value
evalBoolBoolBinOp op x y = let
  Hir.VBool x' = x
  Hir.VBool y' = y
  in Hir.VBool $ x' `op` y'

xor a b = a || b && (not $ a && b)

evalValueBoolBinOp :: (Hir.Value -> Hir.Value -> Bool) -> Hir.Value -> Hir.Value -> Hir.Value
evalValueBoolBinOp op x y = Hir.VBool $ x `op` y

evalTextTextBinOp :: (String -> String -> String) -> Hir.Value -> Hir.Value -> Hir.Value
evalTextTextBinOp op x y = let
  Hir.VText x' = x
  Hir.VText y' = y
  in Hir.VText $ x' `op` y'
