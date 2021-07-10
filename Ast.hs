{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ast
  ( Ast
  , TypedAst(..)
  , ItemG(..)
  , Item(..)
  , TypedItem(..)
  , itemName
  , BinOp(..)
  , ArithOp(..)
  , BoolOp(..)
  , RelOp(..)
  , OtherOp(..)
  , Lit(..)
  , UnaryOp(..)
  , ExprF(..)
  , IsVoid(..)
  , Fix(..)
  , Expr
  , pattern Var
  , pattern Literal
  , pattern Unary
  , pattern Binary
  , pattern Block
  , pattern Call
  , pattern Intrinsic
  , pattern Let
  , pattern Assign
  , pattern Ret
  , pattern If
  , pattern While
  , pattern Nop
  , pattern Ann
  , Typed(..)
  , RecTyped(..)
  , TypedExpr
  )
where

import qualified Ty
import qualified Text.ParserCombinators.Parsec.Pos as Parsec

type Ast = [Item]
type TypedAst = Typed [TypedItem]

data ItemG e = Def String [(String, Ty.Ty)] (e, Maybe Ty.Ty)
type Item = ItemG Expr
type TypedItem = Typed (ItemG TypedExpr)

instance (Show e) => Show (ItemG e) where
  show (Def name paramsAndTys (body, Just retTy)) =
    "def " ++ name ++ show paramsAndTys ++ " -> " ++ show retTy ++ show body
  show (Def name paramsAndTys (body, Nothing)) =
    "def " ++ name ++ show paramsAndTys ++ " = " ++ show body

itemName :: ItemG e -> String
itemName (Def name _ _) = name

data BinOp
  = ArithOp ArithOp
  | BoolOp BoolOp
  | RelOp RelOp
  | OtherOp OtherOp
  deriving (Show)

data ArithOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

data BoolOp
  = And
  | Or
  | Xor
  deriving (Show)

data RelOp
  = Gt
  | Lt
  | Eq
  | Neq
  deriving (Show)

data OtherOp
  = Concat
  deriving (Show)

data Lit
  = Bool Bool
  | Int Integer
  | String String
  | Unit
  deriving (Show)

data UnaryOp
  = Not
  | Neg
  deriving (Show)

data IsVoid = IsVoid | NotVoid
  deriving (Show)

-- This type used to be called `Expr` (see [this commit](1)), but was rewritten
-- to use an F-Algebra (I think that's right?), and backwards-compatible
-- patterns were added according to [this article](2).
-- [1]: https://github.com/eignnx/funlang/blob/9523a850776b9137f9a1c8295e4ee1100edeb98c/Ast.hs#L65-L73
-- [2]: https://mpickering.github.io/posts/2014-11-27-pain-free.html
data ExprF r
  = VarF String
  | LiteralF Lit
  | UnaryF UnaryOp r
  | BinaryF BinOp r r
  | BlockF IsVoid [r]
  | CallF r [r]
  | IntrinsicF Parsec.SourcePos String [r]
  | LetF String r
  | AssignF String r
  | RetF r
  | IfF r r r
  | WhileF r r
  | NopF
  | AnnF r Ty.Ty

newtype Fix f = Fix (f (Fix f))

type Expr = Fix ExprF

pattern Var :: String -> Expr
pattern Var name = Fix (VarF name)
pattern Literal :: Lit -> Expr
pattern Literal x = Fix (LiteralF x)
pattern Unary :: UnaryOp -> Expr -> Expr
pattern Unary op x = Fix (UnaryF op x)
pattern Binary :: BinOp -> Expr -> Expr -> Expr
pattern Binary op x y = Fix (BinaryF op x y)
pattern Block :: IsVoid -> [Expr] -> Expr
pattern Block v es = Fix (BlockF v es)
pattern Call :: Expr -> [Expr] -> Expr
pattern Call f args = Fix (CallF f args)
pattern Intrinsic :: Parsec.SourcePos -> String -> [Expr] -> Expr
pattern Intrinsic pos name args = Fix (IntrinsicF pos name args)
pattern Let :: String -> Expr -> Expr
pattern Let name expr = Fix (LetF name expr)
pattern Assign :: String -> Expr -> Expr
pattern Assign name expr = Fix (AssignF name expr)
pattern Ret :: Expr -> Expr
pattern Ret x = Fix (RetF x)
pattern If :: Expr -> Expr -> Expr -> Expr
pattern If cond yes no = Fix (IfF cond yes no)
pattern While :: Expr -> Expr -> Expr
pattern While cond body = Fix (WhileF cond body)
pattern Nop :: Expr
pattern Nop = Fix NopF
pattern Ann :: Expr -> Ty.Ty -> Expr
pattern Ann expr ty = Fix (AnnF expr ty)

data Typed a = a `HasTy` Ty.Ty
data RecTyped f =  (f (RecTyped f)) `RecHasTy` Ty.Ty

type TypedExpr = RecTyped ExprF

instance (Show (f ExprF)) => Show (ExprF (f ExprF)) where
  show (VarF name) = name
  show (LiteralF (Int x)) = show x
  show (LiteralF (Bool x)) = if x then "true" else "false"
  show (LiteralF (String x)) = show x
  show (UnaryF Not x) = "not " ++ show x
  show (UnaryF Neg x) = "-(" ++ show x ++ ")"
  show (BinaryF op x y) = case op of
    ArithOp Add    -> show x ++ " + " ++ show y
    ArithOp Sub    -> show x ++ " - " ++ show y
    ArithOp Mul    -> show x ++ " * " ++ show y
    ArithOp Div    -> show x ++ " / " ++ show y
    BoolOp And     -> show x ++ " and " ++ show y
    BoolOp Or      -> show x ++ " or " ++ show y
    BoolOp Xor     -> show x ++ " xor " ++ show y
    RelOp Gt       -> show x ++ " > " ++ show y
    RelOp Lt       -> show x ++ " < " ++ show y
    RelOp Eq       -> show x ++ " == " ++ show y
    RelOp Neq      -> show x ++ " != " ++ show y
    OtherOp Concat -> show x ++ " ++ " ++ show y
  show (BlockF isVoid es) = "do\n" ++ unlines (map ((++";") . ("\t"++) . show) es) ++ "end"
  show (CallF f args) = show f ++ show args
  show (IntrinsicF pos name args) = name ++ show args
  show (LetF name e) = "let " ++ name ++ " = " ++ show e
  show (AssignF name e) = name ++ " = " ++ show e
  show (RetF e) = "ret " ++ show e
  show (IfF cond yes no) = "if " ++ show cond ++ " then " ++ show yes ++ " else " ++ show no ++ "end"
  show (WhileF cond body) = "while " ++ show cond ++ show body
  show NopF = "nop"
  show (AnnF e t) = show e ++ ": " ++ show t

instance Show TypedExpr where
  show (e `RecHasTy` t) = "(" ++ show e ++ ") : " ++ show t

instance Show Expr where
  show (Fix e) = show e