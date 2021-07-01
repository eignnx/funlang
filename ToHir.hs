module ToHir
    ( initialCState
    , astToHir
    , runCompilation
    , getDefs
    )
where

import qualified Ast
import qualified Hir
import qualified Intr

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map.Strict               as M

data CState = CState
  { lbl_  :: Hir.Lbl
  , defs_ :: [(String, Hir.Lbl)]
  }
  deriving Show

type CompState = State CState

fresh :: CompState Hir.Lbl
fresh = do
  lbl <- gets lbl_
  modify (\s -> s { lbl_ = lbl + 1 })
  return lbl

define :: String -> Hir.Lbl -> CompState ()
define name lbl = do
  defs <- gets defs_
  modify $ \st -> st { defs_ = (name, lbl) : defs }
  return ()

class Compile a where
  compile :: a -> CompState [Hir.Instr]

instance Compile a => Compile [a] where
  compile []       = return []
  compile (x : xs) = do
    x'  <- compile x
    xs' <- compile xs
    return (x' ++ xs')

instance Compile Ast.Item where
  compile (Ast.Def name params body) = do
    lbl <- fresh
    define name lbl
    -- We gotta use Hir.Swap before store because the RETURN ADDRESS is on TOS!
    let paramBindings = reverse params >>= \param -> [Hir.Swap, Hir.Store param]
    let prologue = paramBindings -- First thing we do is store args (from stack) in memory.
    body' <- compile body
    let epilogue = if name == "main"
                      then [Hir.Intrinsic Intr.Exit]
                      else [Hir.Ret] -- At the end of (almost) every function MUST be a return instr.
    return $  [Hir.Label lbl] -- Label the function.
           ++ prologue -- First run the prologue.
           ++ body' -- Then run the body of the function.
           ++ epilogue -- Finally, run the epilogue.

instance Compile Ast.Stmt where
  compile Ast.Skip              = return []
  compile (Ast.Assign var expr) = do
    expr' <- compile expr
    return $ expr' ++ [Hir.Store var]
  compile (Ast.If cond yes no) = do
    cond'  <- compile cond
    yes'   <- compile yes
    noLbl  <- fresh
    no'    <- compile no
    endLbl <- fresh
    return
      $  cond'
      ++ [Hir.JmpIfFalse noLbl]
      ++ yes'
      ++ [Hir.Jmp endLbl]
      ++ [Hir.Label noLbl]
      ++ no'
      ++ [Hir.Label endLbl]
  compile (Ast.While cond body) = do
    cond' <- compile cond
    top   <- fresh
    body' <- compile body
    end   <- fresh
    return
      $  [Hir.Label top]
      ++ cond'
      ++ [Hir.JmpIfFalse end]
      ++ body'
      ++ [Hir.Jmp top]
      ++ [Hir.Label end]
  compile (Ast.Expr expr) = compile expr

instance Compile Ast.ArithOp where
  compile op = return $ case op of
    Ast.Add -> [Hir.Add]
    Ast.Sub -> [Hir.Sub]
    Ast.Mul -> [Hir.Mul]
    Ast.Div -> [Hir.Div]

instance Compile Ast.Expr where
  compile (Ast.Block []            ) = return []
  compile (Ast.Block (stmt : stmts)) = do
    stmt'  <- compile stmt
    stmts' <- compile (Ast.Block stmts)
    return $ stmt' ++ stmts'
  compile (Ast.Var     name ) = return [Hir.Load name]
  compile (Ast.Literal lit  ) = return [Hir.Const (valueFromLit lit)]
  compile (Ast.Unary op expr) = do
    expr' <- compile expr
    op'   <- compile op
    return $ expr' ++ op'
  compile (Ast.Binary op x y) = do
    y'  <- compile y
    x'  <- compile x
    op' <- compile op
    -- NOTE: you gotta reverse these args below!
    return $ y' ++ x' ++ op'

  compile (Ast.Call fn args) = do
    args' <- compile args -- Using `instance Compile a => Compile [a]`
    fn' <- compile fn
    let argC = length args
    return $ args' -- Code to push the arguments
           ++ fn' -- Code to load the function pointer
           ++ [Hir.Call argC]

  compile (Ast.Intrinsic pos name args) = do
    args' <- mapM compile args
    let intr = Intr.fromName name pos
    return $ join args' ++ [Hir.Intrinsic intr]

instance Compile Ast.UnaryOp where
  compile Ast.Not = return [Hir.Not]
  compile Ast.Neg = return [Hir.Neg]

valueFromLit :: Ast.Lit -> Hir.Value
valueFromLit (Ast.Int    x) = Hir.VInt $ fromIntegral x
valueFromLit (Ast.Bool   x) = Hir.VBool x
valueFromLit (Ast.String x) = Hir.VString x

instance Compile Ast.BinOp where
  compile (Ast.ArithOp op) = compile op
  compile (Ast.BoolOp  op) = compile op
  compile (Ast.RelOp   op) = compile op

instance Compile Ast.BoolOp where
  compile op = return $ case op of
    Ast.And -> [Hir.And]
    Ast.Or  -> [Hir.Or]
    -- A xor B   = (A or B) and (not (A and B))
    -- [postfix] = (A B or) ((A B and) not) and
    Ast.Xor ->
      [ Hir.Over
      , Hir.Over
      , Hir.Or
      , Hir.Rot
      , Hir.Rot
      , Hir.And
      , Hir.Not
      , Hir.And
      ]

instance Compile Ast.RelOp where
  compile op = return $ case op of
    Ast.Gt  -> [Hir.Gt]
    Ast.Lt  -> [Hir.Lt]
    Ast.Eq  -> [Hir.Eq]
    Ast.Neq -> [Hir.Eq, Hir.Not]

addEntryPointJump :: [Hir.Instr] -> [(String, Hir.Lbl)] -> [Hir.Instr]
addEntryPointJump hir defs = globalDefs ++ entryPointJump ++ hir
  where globalDefs = defs >>= (\(name, lbl) -> [Hir.Const (Hir.VLbl lbl), Hir.Store name])
        entryPointJump = [Hir.Jmp mainLbl]
        Just mainLbl = lookup "main" defs

initialCState = CState { lbl_ = Hir.Lbl 0, defs_ = [] }

astToHir :: Compile a => a -> [Hir.Instr]
astToHir ast = addEntryPointJump hir defs
  where (hir, CState { defs_ = defs }) = runCompilation ast

runCompilation :: Compile a => a -> ([Hir.Instr], CState)
runCompilation ast = runState (compile ast) initialCState

getDefs ast = defs
  where (instrs, CState { defs_ = defs }) = runCompilation ast