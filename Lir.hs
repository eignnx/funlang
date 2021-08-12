{-# LANGUAGE LambdaCase #-}

module Lir
  ( InstrAddr(..)
  , Value(..)
  , dbgValue
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
  | VText String
  | VInstrAddr InstrAddr
  | VPtr Int
  deriving (Show, Eq)

dbgValue :: Value -> IO ()
dbgValue (VInt    x) = print x
dbgValue (VBool   x) = print x
dbgValue (VText x) = print x

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
  | Alloc Int
  | MemWriteDirect Int
  | MemReadDirect Int
  | TestDiscr Int
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
    Gt -> "Gt"
    Lt -> "Lt"
    Concat -> "Concat"
    Alloc n -> "Alloc" +++ show n
    MemWriteDirect n -> "MemWriteDirect" +++ show n
    MemReadDirect n -> "MemReadDirect" +++ show n
    TestDiscr d -> "TestDiscr" +++ show d
    Nop -> "Nop"
    JmpIfFalse addr -> "JmpIfFalse" +++ show addr
    Jmp addr -> "Jmp" +++ show addr
    Intrinsic intr -> "Intrinsic" +++ show intr
    Call argc -> "Call" +++ show argc
    CallDirect addr argc -> "CallDirect" +++ show addr +++ show argc
    Ret -> "Ret"