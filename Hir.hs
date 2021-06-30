module Hir
  ( Lbl(..)
  , Value(..)
  , Instr(..)
  )
where

import qualified Intr

newtype Lbl = Lbl Int
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
  | NewFrame -- Allocates a new call frame, sets it as current frame
  | Param -- Pop TOS, stores it in current call frame.
  deriving (Show)
