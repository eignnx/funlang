{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module TypedAstToHir
    ( initialCState
    , astToHir
    , runCompilation
    )
where

import qualified Ty
import           Ty                           ( (<:) )
import qualified Ast
import           Ast                          ( Typed(..) )
import qualified Hir
import           Cata                         ( RecTyped(..) )
import qualified Comptime
import qualified Intr
import Utils ( (+++), codeIdent )

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map.Strict               as M
import           Data.List                     ( find )
import           Debug.Trace                   ( trace )

data CState = CState
  { lbl_  :: Hir.Lbl
  , defs_ :: [(String, Hir.Lbl, [Hir.Instr])]
  }
  deriving Show

type CompState = State CState

fresh :: CompState Hir.Lbl
fresh = do
  lbl <- gets lbl_
  modify (\s -> s { lbl_ = lbl + 1 })
  return lbl

declareFn :: String -> CompState ()
declareFn name = do
  -- First bind the name to a fresh lbl, store it in definitions.
  lbl <- fresh
  defs <- gets defs_
  modify $ \st -> st { defs_ = (name, lbl, []) : defs }

defineFn :: String -> CompState [Hir.Instr] -> CompState Hir.Lbl
defineFn name compilation = do
  lbl <- lookupFnLbl name
  -- Then compile body and update hir in definition.
  hir <- compilation
  defs <- gets defs_
  let hir' = Hir.Label lbl : hir -- Label the function.
  modify $ \st -> st { defs_ = (name, lbl, hir') : defs }
  return lbl

lookupFnLbl :: String -> CompState Hir.Lbl
lookupFnLbl name = do
  defs <- gets defs_
  case find (\(n, _, _) -> n == name) defs of
    Just (_, lbl, _) -> return lbl
    Nothing -> error $ "Internal Compiler Error: Unknown fn binding" +++ codeIdent name

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

instance Compile (Ast.Seq Ast.TypedExpr) where

  compile Ast.Empty = return []

  compile (Ast.Result expr) = compile expr

  compile (Ast.Semi expr@(exprF :<: ty) seq)
    | ty <: Ty.VoidTy = do -- Already Void type, no need to Pop.
      expr' <- compile expr
      seq' <- compile seq
      return $ expr' ++ seq'
    | otherwise = do -- This one needs to discard its non-Void result.
      expr' <- compile expr
      seq' <- compile seq
      let maybePop = if ty <: Ty.VoidTy || Ast.isModLevelItem exprF
                      then []
                      else trace (">>>>>>>>>>>>>>>>>>>>>>>>Adding pop after: " ++ show expr) [Hir.Pop]
      return $ expr' ++ maybePop ++ seq'

instance Compile Ast.TypedExpr where

  compile (Ast.BlockF seq :<: ty) = compile seq

  compile (Ast.CallF (Ast.VarF name :<: f) args :<: resTy) = do
    args' <- compile args -- Using `instance Compile a => Compile [a]`
    lbl <- lookupFnLbl name -- FIXME: Ensure this works when fn names are shadowed.
    let argC = length args
    return $ args' -- Code to push the arguments
           ++ [Hir.CallDirect lbl argC]

  compile (Ast.VarF name :<: ty)
    | ty <: Ty.VoidTy = return []
    | otherwise = return [Hir.Load name]

  compile (Ast.LiteralF lit :<: ty) =
    case lit of
      Ast.Int    x    -> return $ pure $ Hir.Const $ Hir.VInt $ fromIntegral x
      Ast.Bool   x    -> return $ pure $ Hir.Const $ Hir.VBool x
      Ast.String x    -> return $ pure $ Hir.Const $ Hir.VString x
      Ast.Pair (a, b) -> do
        a' <- compile a
        b' <- compile b
        return $ [Hir.Alloc 2] ++ a' ++ [Hir.MemWrite 0] ++ b' ++ [Hir.MemWrite 1]

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

  compile (Ast.RetF expr :<: ty) = compile expr

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
    return [ Hir.Const $ Hir.VString "<Void>"
           , Hir.Intrinsic intr
           ]

  compile (Ast.IntrinsicF pos name args :<: ty) = do
    args' <- mapM compile args
    let intr = Intr.fromName name pos
    return $ join args' ++ [Hir.Intrinsic intr]

  compile (Ast.NopF :<: ty)            = return [Hir.Nop]

  compile (Ast.AnnF expr _ :<: ty)   = compile expr

  compile (Ast.LetF name expr@(_ :<: exprTy) :<: _)
    | exprTy <: Ty.VoidTy = compile expr -- Still gotta run it cause it might have side-effects.
    | otherwise = do
      expr' <- compile expr
      return $ expr' ++ [Hir.Store name]

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

  compile (Ast.ModF name items :<: ty) = do

    -- First, declare all the sub-items so that they can be called in any order.
    forM_ items $ \(itemF :<: _) -> do
      let Just name = Ast.itemName itemF
      declareFn name

    -- Then run the actual compilation.
    forM_ items $ \item@(itemF :<: _) -> do
      if Ast.isModLevelItem itemF
        then compile item
        else error $ "Can't compile `" ++ show item ++ "` as a mod-level item!"
    return []

  compile (Ast.DefF name paramsAndTypes retTy body :<: ty) = do
    fnLbl <- defineFn name $ do
      let params = map fst paramsAndTypes
      let paramBindings = map Hir.Store $ reverse params
      let prologue = paramBindings -- First thing we do is store args (from stack) in memory.
      body' <- compile body
      let epilogue = [Hir.Ret] -- At the end of every function MUST be a return instr.
      return $ prologue -- First run the prologue.
            ++ body'    -- Then run the body of the function.
            ++ epilogue -- Finally, run the epilogue.
    return []

instance Compile Ast.UnaryOp where
  compile Ast.Not = return [Hir.Not]
  compile Ast.Neg = return [Hir.Neg]
  compile (Ast.TupleProj idx) = return [Hir.MemRead $ fromIntegral idx]

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

initialCState = CState { lbl_ = Hir.Lbl 0, defs_ = [] }

runCompilation :: Compile a => a -> ([Hir.Instr], CState)
runCompilation ast = runState (compile ast) initialCState

trampoline :: [(String, Hir.Lbl, [Hir.Instr])] -> [Hir.Instr]
trampoline defs = entryPointJump ++ exit
  where
    exit = [Hir.Intrinsic Intr.Exit]
    entryPointJump = [Hir.CallDirect mainLbl 0]
      where
        mainLbl = case find (\(name, _, _) -> name == "main") defs of
                    Just (_, lbl, _) -> lbl
                    Nothing -> error $ "Couldn't find `def main`!"


joinDefs :: [(String, Hir.Lbl, [Hir.Instr])] -> [Hir.Instr]
joinDefs = concatMap (\(_, _, hir) -> hir)

astToHir :: Compile a => a -> [Hir.Instr]
astToHir ast = trampoline defs ++ joinDefs defs
  where (_hir, CState { defs_ = defs }) = runCompilation ast
