{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module TypedAstToHir
    ( initialCState
    , astToHir
    , runCompilation
    , getDefs
    )
where

import qualified Ty
import           Ty                           ( (<:) )
import qualified Ast
import           Ast                          ( Typed(..), RecTyped(..) )
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
  

instance Compile Ast.ArithOp where
  compile op = return $ case op of
    Ast.Add -> [Hir.Add]
    Ast.Sub -> [Hir.Sub]
    Ast.Mul -> [Hir.Mul]
    Ast.Div -> [Hir.Div]

instance Compile Ast.OtherOp where
  compile Ast.Concat = return [Hir.Concat]

-- | Checks for expressions whose type is not `Void`. If it finds any, inserts
--   `Hir.Pop` afterwards to discard the value.
compileTerminatedExprs :: [Ast.TypedExpr] -> CompState [Hir.Instr]
compileTerminatedExprs exprs = do
  hir <- forM exprs $ \expr@(_ :<: exprTy) -> do
    expr' <- compile expr
    let maybePop = if exprTy <: Ty.VoidTy then [] else [Hir.Pop]
    return $ expr' ++ maybePop
  return $ concat hir

instance Compile (Ast.Seq Ast.TypedExpr) where
  
  compile (Ast.Empty) = return []

  compile (Ast.Result expr) = compile expr

  compile (Ast.Semi expr@(_ :<: ty) seq)
    | ty <: Ty.VoidTy = do -- Already Void type, no need to Pop.
      expr' <- compile expr
      seq' <- compile seq
      return $ expr' ++ seq'
    | otherwise = do -- This one needs to discard its non-Void result.
      expr' <- compile expr
      seq' <- compile seq
      return $ expr' ++ [Hir.Pop] ++ seq'

instance Compile Ast.TypedExpr where

  compile (Ast.BlockF seq :<: ty) = compile seq

  compile (Ast.VarF name :<: ty) =
    if ty <: Ty.VoidTy then
      return []
    else
      return [Hir.Load name]

  compile (Ast.LiteralF lit   :<: ty) = return [Hir.Const (valueFromLit lit)]

  compile (Ast.UnaryF op expr :<: ty) = do
    expr' <- compile expr
    op'   <- compile op
    return $ expr' ++ op'

  compile (Ast.BinaryF op x y :<: ty) = do
    y'  <- compile y
    x'  <- compile x
    op' <- compile op
    -- NOTE: you gotta reverse these args below!
    return $ y' ++ x' ++ op'

  compile (Ast.RetF expr :<: ty) = do
    expr' <- compile expr
    return expr'

  compile (Ast.CallF fn args :<: ty) = do
    args' <- compile args -- Using `instance Compile a => Compile [a]`
    fn' <- compile fn
    let argC = length args
    return $ args' -- Code to push the arguments
           ++ fn' -- Code to load the function pointer
           ++ [Hir.Call argC]

  -- FIXME: HACK!
  -- Intercept the call to deal with `Void` specially. https://pbs.twimg.com/media/EU0GDTVU4AY73KC?format=jpg&name=small
  compile intr@(Ast.IntrinsicF pos "print" [arg@(_ :<: Ty.VoidTy)] :<: ty) = do
    let intr = Intr.fromName "print" pos
    return $  [ Hir.Const $ Hir.VString "<Void>"
              , Hir.Intrinsic intr
              ]

  compile (Ast.IntrinsicF pos name args :<: ty) = do
    args' <- mapM compile args
    let intr = Intr.fromName name pos
    return $ join args' ++ [Hir.Intrinsic intr]

  compile (Ast.NopF :<: ty)            = return [Hir.Nop]

  compile (Ast.AnnF expr _ :<: ty)   = compile expr

  compile (Ast.LetF var expr@(_ :<: exprTy) :<: ty) = do
    expr' <- compile expr
    let maybeStore = if exprTy <: Ty.VoidTy then [] else [Hir.Store var]
    return $ expr' ++ maybeStore

  compile (Ast.AssignF var expr@(_ :<: exprTy) :<: ty) = do
    expr' <- compile expr
    let maybeStore = if exprTy <: Ty.VoidTy then [] else [Hir.Store var]
    return $ expr' ++ maybeStore

  compile (Ast.IfF cond yes no :<: ty) = do
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

  compile (Ast.WhileF cond body :<: ty) = do
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

  compile (Ast.LoopF body :<: ty) = do
    top   <- fresh
    body' <- compile body
    return
      $  [Hir.Label top]
      ++ body'
      ++ [Hir.Jmp top]

  compile (Ast.ModF name items :<: ty) = compile items

  compile (Ast.DefF name paramsAndTypes retTy body :<: ty) = do
    let params = map fst paramsAndTypes
    lbl <- fresh
    define name lbl
    let paramBindings = map Hir.Store $ reverse params
    let prologue = paramBindings -- First thing we do is store args (from stack) in memory.
    body' <- compile body
    let epilogue = [Hir.Ret] -- At the end of every function MUST be a return instr.
    return $  [Hir.Label lbl] -- Label the function.
           ++ prologue -- First run the prologue.
           ++ body' -- Then run the body of the function.
           ++ epilogue -- Finally, run the epilogue.

instance Compile Ast.UnaryOp where
  compile Ast.Not = return [Hir.Not]
  compile Ast.Neg = return [Hir.Neg]

valueFromLit :: Ast.Lit -> Hir.Value
valueFromLit (Ast.Int    x) = Hir.VInt $ fromIntegral x
valueFromLit (Ast.Bool   x) = Hir.VBool x
valueFromLit (Ast.String x) = Hir.VString x

instance Compile Ast.BinOp where
  compile (Ast.ArithOp   op) = compile op
  compile (Ast.BoolOp    op) = compile op
  compile (Ast.RelOp     op) = compile op
  compile (Ast.OtherOp   op) = compile op

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

trampoline :: [(String, Hir.Lbl)] -> [Hir.Instr]
trampoline defs = globalDefs ++ entryPointJump ++ exit
  where
    exit = [Hir.Intrinsic Intr.Exit]
    globalDefs = defs >>= (\(name, lbl) -> [Hir.Const (Hir.VLbl lbl), Hir.Store name])
    entryPointJump = [Hir.Const (Hir.VLbl mainLbl), Hir.Call 0]
      where
        Just mainLbl = lookup "main" defs

astToHir :: Compile a => a -> [Hir.Instr]
astToHir ast = trampoline defs ++ hir
  where (hir, CState { defs_ = defs }) = runCompilation ast

initialCState = CState { lbl_ = Hir.Lbl 0, defs_ = [] }

runCompilation :: Compile a => a -> ([Hir.Instr], CState)
runCompilation ast = runState (compile ast) initialCState

getDefs ast = defs
  where (instrs, CState { defs_ = defs }) = runCompilation ast