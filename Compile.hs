module Compile where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map.Strict               as M
import qualified Parser                        as P

newtype Lbl = Lbl Int
  deriving (Show, Eq, Ord)

data Value = VInt Integer
           | VBool Bool
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
  | Intrinsic Intrinsic
  deriving (Show)

data Intrinsic = Print
  deriving Show

type CompState = State Lbl

fresh :: CompState Lbl
fresh = do
  lbl <- get
  let Lbl x = lbl
  put (Lbl (x + 1))
  return lbl

class Compile a where
  compile :: a -> CompState [Instr]

instance Compile P.Stmt where
  {-
      data Stmt = Seq [Stmt]
              | Assign String AExpr
              | If BExpr Stmt Stmt
              | While BExpr Stmt
              | Skip
  -}
  compile (P.Seq []            ) = return []
  compile (P.Seq (stmt : stmts)) = do
    stmt'  <- compile stmt
    stmts' <- compile (P.Seq stmts)
    return $ stmt' ++ stmts'
  compile P.Skip              = return []
  compile (P.Assign var expr) = do
    expr' <- compile expr
    return $ expr' ++ [Compile.Store var]
  compile (P.If cond yes no) = do
    cond'  <- compile cond
    yes'   <- compile yes
    noLbl  <- fresh
    no'    <- compile no
    endLbl <- fresh
    return
      $  cond'
      ++ [Compile.JmpIfFalse noLbl]
      ++ yes'
      ++ [Compile.Jmp endLbl]
      ++ [Compile.Label noLbl]
      ++ no'
      ++ [Compile.Label endLbl]
  compile (P.While cond body) = do
    cond' <- compile cond
    top   <- fresh
    body' <- compile body
    end   <- fresh
    return
      $  [Compile.Label top]
      ++ cond'
      ++ [Compile.JmpIfFalse end]
      ++ body'
      ++ [Compile.Jmp top]
      ++ [Compile.Label end]
  compile (P.Intrinsic name args) = do
    args' <- mapM compile args -- TODO: remove head
    return $ join args' ++ [Compile.Intrinsic Print]

instance Compile P.ABinOp where
  compile op = return $ case op of
    P.Add -> [Compile.Add]
    P.Sub -> [Compile.Sub]
    P.Mul -> [Compile.Mul]
    P.Div -> [Compile.Div]

instance Compile P.AExpr where
  {-
  data AExpr = Var String
             | IntConst Integer
             | Neg AExpr
             | ABinary ABinOp AExpr AExpr
             deriving (Show)

  data ABinOp = Add
              | Subtract
              | Multiply
              | Divide
              deriving (Show)
      -}
  compile (P.Var      name) = return [Compile.Load name]
  compile (P.IntConst x   ) = return [Compile.Const $ VInt x]
  compile (P.Neg      expr) = do
    expr' <- compile expr
    return $ expr' ++ [Compile.Neg]
  compile (P.ABinary op x y) = do
    x'  <- compile x
    y'  <- compile y
    op' <- compile op
    return $ x' ++ y' ++ op'

instance Compile P.BExpr where
  {-
  data BExpr = BoolConst Bool
             | Not BExpr
             | BBinary BBinOp BExpr BExpr
             | RBinary RBinOp AExpr AExpr
               deriving (Show)

  data BBinOp = And | Or | Xor deriving (Show)

  data RBinOp = Greater
              | Less
              | Equal
              | NotEqual
                deriving (Show)
      -}
  compile (P.BoolConst b    ) = return [Compile.Const $ VBool b]
  compile (P.Not       bexpr) = do
    bexpr' <- compile bexpr
    return $ bexpr' ++ [Compile.Not]
  compile (P.BBinary op x y) = do
    x'  <- compile x
    y'  <- compile y
    op' <- compile op
    return $ x' ++ y' ++ op'
  compile (P.RBinary op x y) = do
    x'  <- compile x
    y'  <- compile y
    op' <- compile op
    return $ x' ++ y' ++ op'

instance Compile P.BBinOp where
  compile op = return $ case op of
    P.And -> [Compile.And]
    P.Or  -> [Compile.Or]
    -- A xor B   = (A or B) and (not (A and B))
    -- [postfix] = (A B or) ((A B and) not) and
    P.Xor ->
      [ Compile.Over
      , Compile.Over
      , Compile.Or
      , Compile.Rot
      , Compile.Rot
      , Compile.And
      , Compile.Not
      , Compile.And
      ]

instance Compile P.RBinOp where
  compile op = return $ case op of
    P.Gt  -> [Compile.Gt]
    P.Lt  -> [Compile.Lt]
    P.Eq  -> [Compile.Eq]
    P.Neq -> [Compile.Eq, Compile.Not]

irFromAst :: Compile a => a -> [Instr]
irFromAst ast = evalState (compile ast) (Lbl 0)
