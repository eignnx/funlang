module Hir
  ( Lbl (..),
    Value (..),
    Instr (..),
  )
where

import qualified Intr

newtype Lbl = Lbl Int
  deriving (Show, Eq, Ord)

instance Num Lbl where
  (Lbl x) + (Lbl y) = Lbl (x + y)
  (Lbl x) * (Lbl y) = Lbl (x * y)
  negate (Lbl x) = Lbl (- x)
  abs (Lbl x) = Lbl (abs x)
  signum (Lbl x) = Lbl (signum x)
  fromInteger x = Lbl $ fromInteger x

data Value
  = VInt Int
  | VBool Bool
  | VText String
  | VLbl Lbl
  | VPtr Int
  deriving (Show, Eq)

data Instr
  = Load String
  | Store String
  | Const Value -- Push an immediate value onto stack
  | Dup -- Stack operations
  | Over -- "
  | Rot -- "
  | Swap -- "
  | Pop -- "
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | And
  | Or
  | Not
  | Gt
  | Lt
  | Concat
  | Alloc Int -- `Alloc n` allocates a contiguous block of memory of size `n`.
  | MemWriteDirect Int -- `MemWriteDirect i` performs `(TOS+1)[i] = TOS`.
  | MemReadDirect Int -- `MemReadDirect  i` performs `(TOS)[i]`.
  | TestDiscr Int -- `TestDiscr d` compares the discriminant field of a variant on the TOS against `d`.
  | Nop
  | JmpIfFalse Lbl
  | Jmp Lbl
  | Label Lbl
  | Intrinsic Intr.Intrinsic
  | -- `Call n` calls the TOS function pointer with the remaining `n`
    -- TOS-values as args
    Call Int
  | CallDirect Lbl Int -- Calls a function whose type is `Fixed[a -> b]`.
  | Ret -- Jump back to return address
  | Instr :# String -- Represents a comment in the Hir. Will be dropped in Lir.
  deriving (Show)
