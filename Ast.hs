{-# LANGUAGE PatternSynonyms #-}

module Ast
  ( Ast
  , Item(..)
  , itemName
  , BinOp(..)
  , ArithOp(..)
  , BoolOp(..)
  , RelOp(..)
  , OtherOp(..)
  , Lit(..)
  , UnaryOp(..)
  , Expr(..)
  , IsVoid(..)
  )
where

import qualified Ty
import qualified Text.ParserCombinators.Parsec.Pos as Parsec

type Ast = [Item]

data Item = Def String [(String, Ty.Ty)] (Expr, Maybe Ty.Ty)
  -- deriving (Show)

itemName :: Item -> String
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

-- pattern Arrow t1 t2 = App "->"    [t1, t2]
-- pattern HeadC x <- x:xs where
--  HeadC x = [x]
pattern Var name = Fix (VarF name)
pattern Literal x = Fix (LiteralF x)
pattern Unary op x = Fix (UnaryF op x)
pattern Binary op x y = Fix (BinaryF op x y)
pattern Block v es = Fix (BlockF v es)
pattern Call f args = Fix (CallF f args)
pattern Intrinsic pos name args = Fix (IntrinsicF pos name args)
pattern Let name expr = Fix (LetF name expr)
pattern Assign name expr = Fix (AssignF name expr)
pattern Ret x = Fix (RetF x)
pattern If cond yes no = Fix (IfF cond yes no)
pattern While cond body = Fix (WhileF cond body)
pattern Nop = Fix NopF
pattern Ann expr ty = Fix (AnnF expr ty)

data IsVoid = IsVoid | NotVoid
  deriving (Show)

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

data Annotated f a = (f (Annotated f a)) `Is` a

type Expr = Fix ExprF
type TypedExpr = Annotated ExprF Ty.Ty