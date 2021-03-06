module Compile where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map.Strict               as M
import qualified Parser                        as P
import           Text.ParserCombinators.Parsec.Pos
                                                ( SourcePos
                                                , sourceLine
                                                )

newtype Lbl = Lbl Int
  deriving (Show, Eq, Ord)

data Value
  = VInt Integer
  | VBool Bool
  | VString String
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

data Intrinsic = Print | Here SourcePos
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
  compile (P.Expr expr) = compile expr

instance Compile P.ArithOp where
  compile op = return $ case op of
    P.Add -> [Compile.Add]
    P.Sub -> [Compile.Sub]
    P.Mul -> [Compile.Mul]
    P.Div -> [Compile.Div]

instance Compile P.Expr where
  compile (P.Var     name ) = return [Compile.Load name]
  compile (P.Literal lit  ) = return [Compile.Const (valueFromLit lit)]
  compile (P.Unary op expr) = do
    expr' <- compile expr
    op'   <- compile op
    return $ expr' ++ op'
  compile (P.Binary op x y) = do
    y'  <- compile y
    x'  <- compile x
    op' <- compile op
    -- NOTE: you gotta reverse these args below!
    return $ y' ++ x' ++ op'
  compile (P.Intrinsic pos name args) = do
    args' <- mapM compile args
    return $ join args' ++ [Compile.Intrinsic op]
   where
    op = case name of
      "print" -> Print
      "here"  -> Here pos

instance Compile P.UnaryOp where
  compile P.Not = return [Compile.Not]
  compile P.Neg = return [Compile.Neg]

valueFromLit :: P.Lit -> Compile.Value
valueFromLit (P.Int    x) = Compile.VInt x
valueFromLit (P.Bool   x) = Compile.VBool x
valueFromLit (P.String x) = Compile.VString x

instance Compile P.BinOp where
  compile (P.ArithOp op) = compile op
  compile (P.BoolOp  op) = compile op
  compile (P.RelOp   op) = compile op

instance Compile P.BoolOp where
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

instance Compile P.RelOp where
  compile op = return $ case op of
    P.Gt  -> [Compile.Gt]
    P.Lt  -> [Compile.Lt]
    P.Eq  -> [Compile.Eq]
    P.Neq -> [Compile.Eq, Compile.Not]

irFromAst :: Compile a => a -> [Instr]
irFromAst ast = evalState (compile ast) (Lbl 0)
