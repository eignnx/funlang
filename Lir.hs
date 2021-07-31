{-# LANGUAGE LambdaCase #-}

module Lir
  ( InstrAddr(..)
  , Value(..)
  , displayValue
  , Instr(..)
  )
where

import qualified Hir
import qualified Intr
import           Utils ( (+++) )

newtype InstrAddr = InstrAddr Int
  deriving (Eq, Ord)
  
instance Show InstrAddr where
  show (InstrAddr x) = show x

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
  deriving (Show, Eq)

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
  | Eq
  | Gt
  | Lt
  | Concat
  | Nop -- Used to replace labels
  | JmpIfFalse InstrAddr
  | Jmp InstrAddr
  | Intrinsic Intr.Intrinsic
  | Call Int
  | CallDirect InstrAddr Int
  | Ret -- Jump back to return address

instance Show Instr where
  show = \case
    Load x -> "Load" +++ x
    Store x -> "Store" +++ x
    Const v -> "Const" +++ show v
    Dup -> "Dup"
    Over -> "Over"
    Rot -> "Rot"
    Swap -> "Swap"
    Pop -> "Pop"
    Add -> "Add"
    Sub -> "Sub"
    Mul -> "Mul"
    Div -> "Div"
    Neg -> "Neg"
    And -> "And"
    Or -> "Or"
    Not -> "Not"
    Eq -> "Eq"
    Gt -> "Gt"
    Lt -> "Lt"
    Concat -> "Concat"
    Nop -> "Nop"
    JmpIfFalse addr -> "JmpIfFalse" +++ show addr
    Jmp addr -> "Jmp" +++ show addr
    Intrinsic intr -> "Intrinsic" +++ show intr
    Call argc -> "Call" +++ show argc
    CallDirect addr argc -> "CallDirect" +++ show addr +++ show argc
    Ret -> "Ret"