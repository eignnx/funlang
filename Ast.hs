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
  deriving (Show)

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

data Expr
  = Var String
  | Literal Lit
  | Unary UnaryOp Expr
  | Binary BinOp Expr Expr
  | Block IsVoid [Expr]
  | Call Expr [Expr]
  | Intrinsic Parsec.SourcePos String [Expr]
  | Let String Expr
  | Assign String Expr
  | Ret Expr
  | If Expr Expr Expr
  | While Expr Expr
  | Nop
  | Ann Expr Ty.Ty
  deriving (Show)

data IsVoid = IsVoid | NotVoid
  deriving (Show)
