module Lir
  ( InstrAddr
  , Value(..)
  , displayValue
  , Instr(..)
  )
where

import qualified Hir
import qualified Intr

newtype InstrAddr = InstrAddr Int
  deriving (Show, Eq, Ord)

instance Num InstrAddr where
  (InstrAddr x) + (InstrAddr y) = InstrAddr (x + y)
  (InstrAddr x) * (InstrAddr y) = InstrAddr (x * y)
  negate (InstrAddr x) = InstrAddr (-x)
  abs (InstrAddr x) = InstrAddr (abs x)
  signum (InstrAddr x) = InstrAddr (signum x)
  fromInteger x = InstrAddr $ fromInteger x

data Value
  = VInt Int
  | VBool Bool
  | VString String
  | VInstrAddr InstrAddr
  deriving Show

displayValue :: Value -> IO ()
displayValue (VInt    x) = print x
displayValue (VBool   x) = print x
displayValue (VString x) = print x

data Instr
  = Load String
  | Store String
  | Const Value -- Push an immediate value onto stack
  | Dup -- Stack operations
  | Over -- "
  | Rot -- "
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
  | Nop -- Used to replace labels
  | JmpIfFalse InstrAddr
  | Jmp InstrAddr
  | Intrinsic Intr.Intrinsic
  deriving (Show)
