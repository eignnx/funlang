{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module TastToHir
    ( initialCState
    , astToHir
    , runCompilation
    )
where

import qualified Ty
import           Ty                           ( isZeroSized )
import qualified Ast
import           Ast                          ( Typed(..) )
import qualified Tcx
import qualified Hir
import           Hir                          ( Instr((:#)) )
import           Cata                         ( RecTyped(..) )
import qualified Comptime
import qualified Intr
import Utils ( (+++), codeIdent, optList, braces )

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map.Strict               as M
import           Data.List                     ( find )
import           Debug.Trace                   ( trace )
import Tcx (NoAliasTy(DestructureNoAlias))

type Descriminant = Int

data CState = CState
  { lbl_   :: Hir.Lbl
  , defs_  :: [(String, Hir.Lbl, [Hir.Instr])]
  , vrnts_ :: M.Map (String, [Tcx.NoAliasTy]) Descriminant
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
  let fnLbl = Hir.Label lbl :# ("Start of def" +++ name)
  let hir' = fnLbl : hir -- Label the function.
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

  compile (Ast.Semi expr@(exprF :<: DestructureNoAlias ty) seq)
    | isZeroSized ty = do -- Already Void type, no need to Pop.
      expr' <- compile expr
      seq' <- compile seq
      return $ expr' ++ seq'
    | otherwise = do -- This one needs to discard its non-Void result.
      expr' <- compile expr
      seq' <- compile seq
      let maybePop = [Hir.Pop | not (isZeroSized ty || Ast.isModLevelItem exprF)]
      return $ expr' ++ maybePop ++ seq'

instance Compile Ast.Pat where

  compile = \case

    Ast.VarPat name ->
      return [Hir.Store name]

    Ast.TuplePat ps -> do
      -- For every sub-pattern (except the last one), we'll need a copy of the TOS.
      let dups = replicate (length ps - 1) Hir.Dup

      -- Before the i-th sub-pattern, add `MemReadDirect i`.
      ps' <- forM (zip ps [0..]) $ \(pat, idx) -> do
        pat' <- compile pat
        return $ Hir.MemReadDirect idx : pat'

      return $ dups ++ concat ps'

instance Compile (Ast.RefutPat, Hir.Lbl, NoAliasTy) where

  compile = \case

    (Ast.VarRefutPat name, _, _) ->
      return [Hir.Store name]

    (Ast.VrntRefutPat name params, next, DestructureNoAlias (Ty.VrntTy vrnts)) -> do
      let Just paramTys = map Tcx.unsafeToNoAlias <$> M.lookup name vrnts

      -- Test the discriminant, if test fails, go to to next match arm.
      discr <- genDiscriminant name paramTys
      let testDiscr = [Hir.TestDiscr discr :# ("Discriminant for" +++ braces (name ++ optList paramTys))]
      let jmpIfFalse = [Hir.JmpIfFalse next :# "Jmp to next match arm if test fails"]

      -- Compile all the sub-patterns
      subPats <- forM (zip3 params paramTys [1..]) $ \(refutPat, patTy, idx) -> do
        refutPat' <- compile (refutPat, next, patTy)
        return
          $ (Hir.Dup :# "We need a copy of local scrutinee from which to project")
          : (Hir.MemReadDirect idx :# ("Project the" +++ show (idx-1) ++ "th variant field"))
          : refutPat'

      return
        $  testDiscr
        ++ jmpIfFalse
        ++ concat subPats

instance Compile Ast.TypedExpr where

  compile (Ast.BlockF seq :<: ty) = compile seq

  compile (Ast.CallF (Ast.VarF name :<: f) args :<: resTy) = do
    args' <- compile args -- Using `instance Compile a => Compile [a]`
    lbl <- lookupFnLbl name -- FIXME: Ensure this works when fn names are shadowed.
    let argC = length args
    return $ args' -- Code to push the arguments
           ++ [Hir.CallDirect lbl argC]

  compile (Ast.VarF name :<: DestructureNoAlias ty)
    | isZeroSized ty = return []
    | otherwise = return [Hir.Load name]

  compile (Ast.LiteralF lit :<: ty) =
    case lit of

      Ast.Int  x ->
        return $ pure $ Hir.Const $ Hir.VInt $ fromIntegral x

      Ast.Bool x ->
        return $ pure $ Hir.Const $ Hir.VBool x

      Ast.Text x ->
        return $ pure $ Hir.Const $ Hir.VText x

      Ast.Tuple exprs -> do
        exprs' <- mapM compile exprs
        return $ allocation : writes exprs'
        where
          n = length exprs
          allocation = Hir.Alloc n :# ("Allocate an" +++ show n ++ "-tuple")
          writes :: [[Hir.Instr]] -> [Hir.Instr]
          writes es = concat $ zipWith addWrite es [0..]
          addWrite :: [Hir.Instr] -> Int -> [Hir.Instr]
          addWrite hir idx = hir ++ [Hir.MemWriteDirect idx :# ("Initialize the" +++ show idx ++ "th tuple field")]

      Ast.Vrnt name args -> do
        let argTys = map (\(_ :<: ty) -> ty) args
        desc <- genDiscriminant name argTys
        args' <- mapM compile args
        return $ allocation : tag desc argTys ++ writes args'
        where
          n = length args
          allocation = Hir.Alloc (n + 1) :# ("Allocate variant that has" +++ show n +++ "fields")
          tag desc argTys =
            [Hir.Const (Hir.VInt desc) :# ("Discriminant for" +++ braces (name ++ optList argTys))
            , Hir.MemWriteDirect 0
            ]
          writes :: [[Hir.Instr]] -> [Hir.Instr]
          writes es = concat $ zipWith addWrite es [1..]
          addWrite :: [Hir.Instr] -> Int -> [Hir.Instr]
          addWrite hir idx = hir ++ [Hir.MemWriteDirect idx :# ("Initialize the" +++ show (idx-1) ++ "th variant field")]

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

  compile (Ast.IntrinsicF pos name args :<: ty) = do
    args' <- mapM compile args
    let intr = Intr.fromName name pos
    return $ join args' ++ [Hir.Intrinsic intr]

  compile (Ast.NopF :<: ty) = return [Hir.Nop :# "From `nop` expr"]

  compile (Ast.AnnF expr _ :<: ty) = compile expr

  compile (Ast.LetF pat expr@(_ :<: DestructureNoAlias exprTy) :<: _)
    | isZeroSized exprTy = compile expr -- Still gotta run it cause it might have side-effects.
    | otherwise = do
      expr' <- compile expr
      pat' <- compile pat
      return $ expr' ++ pat'

  compile (Ast.AssignF var expr@(_ :<: DestructureNoAlias exprTy) :<: ty) = do
    expr' <- compile expr
    let maybeStore = [Hir.Store var | not (isZeroSized exprTy)]
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
      ++ [Hir.Label noLbl :# "Else branch"]
      ++ no'
      ++ [Hir.Label endLbl :# "End of `if` expr"]

  compile (Ast.MatchF scrut@(_ :<: patTy) arms :<: ty) = do
    matchEnd <- fresh
    scrut'   <- compile scrut
    arms'    <- forM arms $ \(refutPat, body) -> do
      next      <- fresh
      refutPat' <- compile (refutPat, next, patTy)
      body'     <- compile body
      return
        $  [Hir.Dup :# "Make a copy of scrutinee for next arm"]
        ++ refutPat'
        ++ [Hir.Pop :# "Next arm will not be reached, pop its copy of scrutinee"]
        ++ body'
        ++ [Hir.Jmp matchEnd :# "Break out of match"]
        ++ [Hir.Label next :# "Next match branch"]
    return
      $  scrut'
      ++ concat arms'
      ++ [Hir.Label matchEnd :# "Match end"]

  compile (Ast.WhileF cond body :<: ty) = do
    cond' <- compile cond
    top   <- fresh
    body' <- compile body
    end   <- fresh
    return
      $  [Hir.Label top :# "Top of while loop"]
      ++ cond'
      ++ [Hir.JmpIfFalse end :# "Jmp to end ofwhile loop"]
      ++ body'
      ++ [Hir.Jmp top]
      ++ [Hir.Label end :# "End of while loop"]

  compile (Ast.LoopF body :<: ty) = do
    top   <- fresh
    body' <- compile body
    return
      $  [Hir.Label top :# "Loop top"]
      ++ body'
      ++ [Hir.Jmp top :# "Jump to top of loop"]

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
      let epilogue = [Hir.Ret :# ("End of def" +++ name)] -- At the end of every function MUST be a return instr.
      return $ prologue -- First run the prologue.
            ++ body'    -- Then run the body of the function.
            ++ epilogue -- Finally, run the epilogue.
    return []

  compile (Ast.TyDefF _ name ctorDefs :<: ty) = do
    return []

genDiscriminant :: String -> [NoAliasTy] -> CompState Int
genDiscriminant name argTys = do
  vrnts <- gets vrnts_
  case M.lookup (name, argTys) vrnts of
    Just desc -> return desc
    Nothing -> do
      let desc = M.size vrnts
      let vrnts' = M.insert (name, argTys) desc vrnts
      modify (\st -> st { vrnts_ = vrnts' })
      return desc

instance Compile Ast.UnaryOp where
  compile Ast.Not = return [Hir.Not]
  compile Ast.Neg = return [Hir.Neg]
  compile (Ast.TupleProj idx) = return [Hir.MemReadDirect $ fromIntegral idx]

instance Compile Ast.BinOp where
  compile (Ast.ArithOp op) = compile op
  compile (Ast.BoolOp  op) = compile op
  compile (Ast.RelOp   op) = compile op
  compile (Ast.OtherOp op) = compile op

instance Compile Ast.BoolOp where
  compile op = return $ case op of
    Ast.And -> [Hir.And]
    Ast.Or  -> [Hir.Or]
    -- A xor B   = (A or B) and (not (A and B))
    -- [postfix] = (A B or) ((A B and) not) and
    Ast.Xor ->
      [ Hir.Over :# "Begin Xor"
      , Hir.Over
      , Hir.Or
      , Hir.Rot
      , Hir.Rot
      , Hir.And
      , Hir.Not
      , Hir.And :# "End Xor"
      ]

instance Compile Ast.RelOp where
  compile op = return $ case op of
    Ast.Gt  -> [Hir.Gt]
    Ast.Lt  -> [Hir.Lt]

initialCState = CState { lbl_ = Hir.Lbl 0
                       , defs_ = []
                       , vrnts_ = M.empty
                       }

runCompilation :: Compile a => a -> ([Hir.Instr], CState)
runCompilation ast = runState (compile ast) initialCState

trampoline :: [(String, Hir.Lbl, [Hir.Instr])] -> [Hir.Instr]
trampoline defs = entryPointJump ++ exit
  where
    exit = [Hir.Intrinsic Intr.Exit :# "Upon return from main, exit"]
    entryPointJump = [Hir.CallDirect mainLbl 0 :# "Call main"]
      where
        mainLbl = case find (\(name, _, _) -> name == "main") defs of
                    Just (_, lbl, _) -> lbl
                    Nothing -> error "Couldn't find `def main`!"


joinDefs :: [(String, Hir.Lbl, [Hir.Instr])] -> [Hir.Instr]
joinDefs = concatMap (\(_, _, hir) -> hir)

astToHir :: Compile a => a -> [Hir.Instr]
astToHir ast = trampoline defs ++ joinDefs defs
  where (_hir, CState { defs_ = defs }) = runCompilation ast
