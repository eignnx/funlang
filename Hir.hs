module Hir
  ( Lbl(..)
  , Value(..)
  , Instr(..)
  )
where

import qualified Intr

data Lbl = Lbl Int | HereLbl
  deriving (Show, Eq, Ord)

instance Num Lbl where
  (Lbl x) + (Lbl y) = Lbl (x + y)
  (Lbl x) * (Lbl y) = Lbl (x * y)
  negate (Lbl x) = Lbl (-x)
  abs (Lbl x) = Lbl (abs x)
  signum (Lbl x) = Lbl (signum x)
  fromInteger x = Lbl $ fromInteger x

data Value
  = VInt Int
  | VBool Bool
  | VString String
  | VLbl Lbl
  deriving (Show)

data Instr
  = Load String
  | Store String
  | Const Value -- Push an immediate value onto stack
  | Dup  -- Stack operations
  | Over -- "
  | Rot  -- "
  | Swap -- "
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | And
  | Or
  | Not
  | Eq
  | Gt
  | Lt
  | JmpIfFalse Lbl
  | Jmp Lbl
  | Label Lbl
  | Intrinsic Intr.Intrinsic
  | Call Int -- `Call n` calls the TOS function pointer with the remaining `n`
             -- TOS-values as args
  | Ret -- Jump back to return address
  deriving (Show)
