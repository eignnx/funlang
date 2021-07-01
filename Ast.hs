module Ast
  ( Ast
  , Item(..)
  , BinOp(..)
  , ArithOp(..)
  , BoolOp(..)
  , RelOp(..)
  , OtherOp(..)
  , Lit(..)
  , UnaryOp(..)
  , Expr(..)
  , Stmt(..)
  )
where

import qualified Text.ParserCombinators.Parsec.Pos as Parsec

type Ast = [Item]

data Item = Def String [String] Expr
  deriving (Show)

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
  | Block [Stmt]
  | Call Expr [Expr]
  | Intrinsic Parsec.SourcePos String [Expr]
  deriving (Show)

data Stmt
  = Assign String Expr
  | Ret Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Skip
  | Expr Expr
  deriving (Show)
