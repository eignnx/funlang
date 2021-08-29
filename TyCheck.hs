{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module TyCheck
  ( astToTypedAst
  )
where

import qualified Ast
import Ast ( Typed(HasTy) )
import Ty ( Ty(..) )
import Tcx
    ( mapA,
      resolveAsVrnts,
      inNewScope,
      unsafeToNoAlias,
      toNoAlias,
      initTcx,
      defineTyAlias,
      resolveAliasAsVrnts,
      getFnRetTy,
      setFnRetTy,
      define,
      varLookup,
      (>||<),
      (-&&>),
      (<:),
      NoAliasTy(..),
      TyChecker,
      forA, withErrMsg, pairA )
import Utils ( codeIdent, code, (+++) )
import Cata ( At((:@:)), RecTyped(..) )
import Res ( ensureM, toRes, Error(RootCause), Res )
import qualified Intr
import qualified Data.Map as M
import Control.Monad ( foldM )
import           Data.Semigroup
import Control.Monad.State ( evalStateT, foldM, MonadTrans(lift), StateT (runStateT) )
import           Control.Applicative
import Data.Maybe ( fromMaybe )
import Debug.Trace (trace)

class CheckType a where
  type Checked a :: *
  infer :: a -> TyChecker (Checked a)
  check :: a -> Ty -> TyChecker (Checked a)

instance CheckType Ast.Pat where
  type Checked Ast.Pat = Ast.Pat

  infer = undefined

  check (Ast.VarPat name) ty = do
    define name ty
    return $ Ast.VarPat name

  check pat@(Ast.TuplePat ps) (TupleTy ts)
    | length ps == length ts = do
      let pairs = zip ps ts
      ps <- mapA (uncurry check) pairs
      return $ Ast.TuplePat ps
    | otherwise = fail $ "The pattern" +++ code pat +++ "can't be bound to a tuple that has" +++ show (length ts) +++ "elements"

  check pat@(Ast.TuplePat ps) nonTupleTy =
    fail $ "The tuple pattern" +++ code pat +++ "can't be bound to something of type" +++ code nonTupleTy

instance CheckType Ast.RefutPat where
  type Checked Ast.RefutPat = Ast.RefutPat

  infer = undefined

  check (Ast.VarRefutPat name) ty = do
    define name ty
    return $ Ast.VarRefutPat name

  check pat@(Ast.VrntRefutPat name params) someVrnt = do
    vrnts <- resolveAsVrnts someVrnt
    case M.lookup name vrnts of
      Nothing -> fail msg
        where msg = "I don't know what" +++ codeIdent name +++ "refers to in the pattern" +++ code pat
      Just expectedParamTys -> do
        params' <- forA (zip params expectedParamTys) $ \(param, expectedTy) -> do
          check param expectedTy
        return $ Ast.VrntRefutPat name params'

instance CheckType (Ast.Seq Ast.Expr) where

  type Checked (Ast.Seq Ast.Expr) = (Ast.Seq Ast.TypedExpr, NoAliasTy)

  infer Ast.Empty = do
    return (Ast.Empty, unsafeToNoAlias VoidTy)

  infer (Ast.Result e) = do
    e'@(_ :<: ty) <- infer e
    let result = Ast.Result e'
    return (result, ty)

  infer (Ast.Semi e seq) = do
    e'@(_ :<: DestructureNoAlias eTy) <- infer e
    (seq', DestructureNoAlias seqTy) <- infer seq
    let semi = Ast.Semi e' seq'
    let ty = unsafeToNoAlias $ eTy -&&> seqTy
    return (semi, ty)

  check Ast.Empty ty = ensureM (ty <: VoidTy) msg $ do
    return (Ast.Empty, unsafeToNoAlias VoidTy)
    where msg = "An empty sequence has type" +++ code VoidTy +++ "not" +++ code ty

  check (Ast.Result e) ty = do
    e'@(_ :<: ty) <- check e ty
    let result = Ast.Result e'
    return (result, ty)

  check (Ast.Semi e seq) ty = do
    e'@(_ :<: DestructureNoAlias eTy) <- check e VoidTy
    (seq', DestructureNoAlias seqTy) <- check seq ty
    let semi = Ast.Semi e' seq'
    let ty = unsafeToNoAlias $ eTy -&&> seqTy
    return (semi, ty)

-- | Takes a Foldable sequence of typed exprs and merges their types via `-&&>`.
--   Example:
--     operationalArgsType [a :<: IntTy, b :<: NeverTy, c :<: IntTy] == NeverTy
--     operationalArgsType [a :<: IntTy, b :<: BoolTy] == BoolTy
--     operationalArgsType [] == VoidTy
operationalArgsType :: Foldable t => t (RecTyped f) -> Ty
operationalArgsType = foldr ((-&&>) . getTy) VoidTy
  where getTy (_ :<: ty) = getNoAlias ty

checkArgs :: Ast.Expr -> [Ast.Expr] -> [Ty] -> TyChecker [Ast.TypedExpr]
checkArgs fn args argTys
  | length args == length argTys =
    mapA (uncurry check) (zip args argTys) -- Check that the args have right types.
  | otherwise = fail msg
    where msg = "Oops! You passed" +++ show received +++ "arguments to"
              +++ code fn ++ ", but it expects" +++ show expected
          expected = length argTys
          received = length args

-- Helper function for inferring+checking unary operators.
-- Pass in the expected type of the arguments, the expected return type, and it will do
-- the checking and inference for you.
inferUnaryOp :: Ast.UnaryOp
             -> Ty -- Expected argument type
             -> Ty -- Result type
             -> Ast.Expr
             -> TyChecker Ast.TypedExpr
inferUnaryOp op argTy retTy expr = do
  expr'@(_ :<: exprTy) <- check expr argTy
  let ty = unsafeToNoAlias $ getNoAlias exprTy -&&> retTy
  return (Ast.UnaryF op expr' :<: ty)

-- Helper function for inferring+checking binary operators.
-- Pass in the expected type of the arguments, the expected return type, and it will do
-- the checking and inference for you.
inferBinOp :: Ast.BinOp
           -> Ty -- Expected argument type
           -> Ty -- Result type
           -> Ast.Expr
           -> Ast.Expr
           -> TyChecker Ast.TypedExpr
inferBinOp op argTy retTy e1 e2 = do
    e1'@(_  :<: ty1) <- check e1 argTy
    e2'@(_ :<: ty2) <- check e2 argTy
    let ty = unsafeToNoAlias $ getNoAlias ty1 -&&> getNoAlias ty2 -&&> retTy
    return (Ast.BinaryF op e1' e2' :<: ty)

itemName :: Show (Ast.ExprF r) => Ast.ExprF r -> Res String
itemName expr = Ast.itemName expr `toRes` RootCause msg
  where msg = "The expression" +++ code expr +++ "can't appear in the top level"

instance CheckType Ast.Expr where

  type Checked Ast.Expr = Ast.TypedExpr

  infer (Ast.VarF name :@: loc) = do
    ty <- varLookup name
    tyNoAliasRes <- toNoAlias ty
    let var = Ast.VarF name
    return $ var :<: tyNoAliasRes

  infer (Ast.LiteralF x :@: loc) =
    case x of

      Ast.Unit   -> return $ Ast.LiteralF Ast.Unit :<: unsafeToNoAlias VoidTy

      Ast.Bool b -> return $ Ast.LiteralF (Ast.Bool b) :<: unsafeToNoAlias BoolTy

      Ast.Int i  -> return $ Ast.LiteralF (Ast.Int i) :<: unsafeToNoAlias IntTy

      Ast.Text t -> return $ Ast.LiteralF (Ast.Text t) :<: unsafeToNoAlias TextTy

      Ast.Tuple exprs -> do
        exprs' <- mapA infer exprs
        let exprTys = map (\(_ :<: ty) -> getNoAlias ty) exprs'
        let ty = unsafeToNoAlias $ TupleTy exprTys
        return $ Ast.LiteralF (Ast.Tuple exprs') :<: ty

      -- To infer the type of a variant literal, just pacakge its name and the
      -- types of its arguments inside `VrntTy`.
      Ast.Vrnt name args -> do
        args' <- mapA infer args
        let argTys = map (\(_ :<: ty) -> getNoAlias ty) args'
        let ty = unsafeToNoAlias $ VrntTy $ M.singleton name argTys
        let lit = Ast.LiteralF (Ast.Vrnt name args')
        return $ lit :<: ty

  infer (Ast.UnaryF op@Ast.Not expr :@: loc) = inferUnaryOp op BoolTy BoolTy expr
  infer (Ast.UnaryF op@Ast.Neg expr :@: loc) = inferUnaryOp op IntTy IntTy expr
  infer e@(Ast.UnaryF (Ast.TupleProj idx) expr :@: loc) = do
    expr'@(_ :<: DestructureNoAlias (TupleTy tys)) <- infer expr
    if fromIntegral idx < length tys
      then do
        let ty = unsafeToNoAlias $ tys !! fromIntegral idx
        return $ Ast.UnaryF (Ast.TupleProj idx) expr' :<: ty
      else fail $ "The tuple expression" +++ code expr +++ "has only" +++ show (length tys) +++ "components, so you can't project the component at index" +++ show idx

  infer (Ast.BinaryF op@(Ast.ArithOp _) e1 e2 :@: loc)          = inferBinOp op IntTy IntTy e1 e2
  infer (Ast.BinaryF op@(Ast.BoolOp  _) e1 e2 :@: loc)          = inferBinOp op BoolTy BoolTy e1 e2
  infer (Ast.BinaryF op@(Ast.RelOp _) e1 e2 :@: loc)            = inferBinOp op IntTy BoolTy e1 e2
  infer (Ast.BinaryF op@(Ast.OtherOp Ast.Concat) e1 e2 :@: loc) = inferBinOp op TextTy TextTy e1 e2

  -- This `infer` impl simply defers to `instance CheckType (Ast.Seq Ast.Expr)`.
  infer (Ast.BlockF seq :@: loc) = do
    (seq', ty) <- infer seq
    return $ Ast.BlockF seq' :<: ty

  -- The runtime imposes the following order on the execution of a `Call`
  -- expression:
  --   1. The arguments are evaluated, (arbitrarily) right to left,
  --   2. The callable is evaluated, then
  --   3. The call is performed.
  -- This means that in the call `f[a, b]`, the execution sequence is `a; b; f`.
  -- So if any of `a`, `b`, or `f` have type `Never`, the entire expression does
  -- as well. The type of the `Call` expression is `a -&&> b -&&> f -&&> retTy`.
  --
  -- HOWEVER: While type-checking, we must FIRST infer the type of `f`, THEN
  -- check that the inferred types of the arguments match `f`'s parameter types.
  infer (Ast.CallF fn args :@: loc) = do
    fn'@(_ :<: DestructureNoAlias fnTy) <- infer fn
    case fnTy of
      FnTy paramTys retTy -> do
        args' <- checkArgs fn args paramTys
        let call = Ast.CallF fn' args'
        let ty = operationalArgsType args' -&&> retTy
        tyNoAliasRes <- toNoAlias ty
        return $ call :<: tyNoAliasRes
      NeverTy -> do
        -- Oops! Well, let's make the best of it. Try inferring args.
        args' <- mapA infer args
        return $ Ast.CallF fn' args' :<: unsafeToNoAlias NeverTy
      nonFnTy -> fail $ code fn +++ "is a" +++ code nonFnTy ++ ", not a function"

  infer expr@(Ast.IntrinsicF place name args :@: loc) = do
    let intr = Intr.fromName name place
    let (expectedArgs, expectedRet) = Intr.sig intr
    args' <- checkArgs expr args expectedArgs
    let ty = operationalArgsType args' -&&> expectedRet
    tyNoAlias <- toNoAlias ty
    let intr = Ast.IntrinsicF place name args'
    return $ intr :<: tyNoAlias

  infer (Ast.LetF pat expr :@: loc) = do
    expr'@(_ :<: DestructureNoAlias exprTy) <- infer expr
    pat <- check pat exprTy `withErrMsg` badPat pat
    let ty = unsafeToNoAlias $ exprTy -&&> VoidTy
    let letExpr = Ast.LetF pat expr'
    return (letExpr :<: ty)
    where
      badPat pat = "The declaration of" +++ code pat +++ "doesn't type check"

  infer (Ast.AssignF name expr :@: loc) = do
    varTy <- varLookup name `withErrMsg` badLookup name
    expr'@(_ :<: DestructureNoAlias exprTy) <- check expr varTy `withErrMsg` badVarTy expr name
    let ty = unsafeToNoAlias $ exprTy -&&> VoidTy
    let assign = Ast.AssignF name expr'
    return $ assign :<: ty
    where
      badLookup name = "I can't assign to the undeclared variable" +++ codeIdent name
      badVarTy expr name = "The value" +++ code expr +++ "can't be assigned to variable" +++ codeIdent name

  infer (Ast.LetConstF name expr :@: loc) = do
    expr'@(_ :<: DestructureNoAlias exprTy) <- infer expr `withErrMsg` badInfer name
    define name exprTy
    let ty = unsafeToNoAlias $ exprTy -&&> VoidTy
    let letConst = Ast.LetConstF name expr'
    return $ letConst :<: ty
    where
      badInfer name = "The declaration of" +++ codeIdent name +++ "doesn't type check"

  infer (Ast.RetF expr :@: loc) = do
    fnRetTy <- getFnRetTy
    expr'@(_ :<: DestructureNoAlias exprTy) <- check expr fnRetTy
    let ret = Ast.RetF expr'
    -- The `-&&>` shouldn't be necessary here, but whatever.
    let ty = unsafeToNoAlias $ exprTy -&&> NeverTy
    return $ ret :<: ty

--   -- infer ctx (FnExpr (AnnParam param paramTy) body) =
--   --   let ctx' = (param, paramTy) : ctx
--   --   in case infer ctx' body of
--   --     Ok bodyTy -> Ok $ FnTy paramTy bodyTy
--   --     err -> err
--   -- infer ctx fn@(FnExpr (Infer param) body) =
--   --   Err $ RootCause $ "I can't infer the type of the parameter `" ++ param ++ "` in the function `" ++ show fn ++ "`"

  -- -- To INFER the type of an if expression:
  -- --   1. INFER one of it's branches, then
  -- --   2. CHECK that the whole if expr has that type.
  -- -- NOTE #1: We only need to be able to INFER one (1) of the branches.
  -- -- NOTE #2: The traslation to Ast.TypedExpr will be done by `check`.
  -- infer (Ast.IfF cond yes no :@: loc) = do
  --   condRes <- check cond BoolTy
  --   bodyRes <- checkSameType yes no
  --   case (condRes, bodyRes) of
  --     (Ok cond', Ok (yes', no', bodyTy)) -> do
  --       let _ :<: DestructureNoAlias condTy = cond'
  --       let ifExpr = Ast.IfF cond' yes' no'
  --       tyNoAliasRes <- toNoAlias $ condTy -&&> bodyTy
  --       return $ (ifExpr :<:) <$> tyNoAliasRes
  --     (_, Err err) -> return $ condRes *> (Err err `addError` msg)
  --       where msg = "The two branches of this `if` expression have different types"
  --     _ -> return $ condRes <* bodyRes

  -- A while expression does NOT return the never type. This is because
  -- usually, it does not infinitely loop. It usually loops until the
  -- condition is no longer true, then ends, yielding void.
  infer (Ast.WhileF cond body :@: loc) = do
    cond'@(_ :<: DestructureNoAlias condTy) <- check cond BoolTy `withErrMsg` badCond
    body'@(_ :<: DestructureNoAlias bodyTy) <- check body VoidTy `withErrMsg` badBody
    let while = Ast.WhileF cond' body'
    let ty = unsafeToNoAlias $ condTy -&&> bodyTy
    return $ while :<: ty
    where
      badCond = "The condition of this `while` loop doesn't have type" +++ code BoolTy
      badBody = "The body of a this `while` loop doesn't have type" +++ code VoidTy

  infer (Ast.LoopF body :@: loc) = do
    body' <- check body VoidTy `withErrMsg` badBody
    let loop = Ast.LoopF body'
    return $ loop :<: unsafeToNoAlias NeverTy
    where
      badBody = "The body of this `loop` expression doesn't have type" +++ code VoidTy

  infer (Ast.NopF :@: loc) = return $ Ast.NopF :<: unsafeToNoAlias VoidTy

  infer (Ast.AnnF expr ty :@: loc) = do
    expr' <- check expr ty `withErrMsg` badCheck expr ty
    tyNoAlias <- toNoAlias ty
    return $ Ast.AnnF expr' ty :<: tyNoAlias
    where
      badCheck expr ty = "The annotated expression" +++ code expr +++ "does not have type" +++ code ty

  -- For when the return type IS specified.
  infer (Ast.DefF name params (Just retTy) body :@: loc) = do
    setFnRetTy retTy -- Set fn's return type.
    (paramTys, body'@(_ :<: DestructureNoAlias bodyTy)) <- inNewScope $ do
      paramTys <- mapA (uncurry define) params -- Note: `mapA` not necc'ry here, could use `mapM`.
      body' <- check body retTy `withErrMsg` badBody name
      return (paramTys, body')
    define name $ FnTy paramTys bodyTy -- We MUST save the full type now.
    let def = Ast.DefF name params (Just retTy) body'
    return $ def :<: unsafeToNoAlias VoidTy
    where
      badBody name = "The body of function" +++ codeIdent name +++ "doesn't type check"

  -- For when the return type is NOT specified.
  infer (Ast.DefF name params Nothing body :@: loc) = do
    setFnRetTy NeverTy -- We don't know the fn's return type yet! FIXME: seems bad...
    (paramTys, typedBody@(_ :<: DestructureNoAlias bodyTy)) <- inNewScope $ do
      paramTys <- mapA (uncurry define) params -- Note: `mapA` not necc'ry here, could use `mapM`.
      bodyRes <- infer body `withErrMsg` badBody name
      return (paramTys, bodyRes)
    define name $ FnTy paramTys bodyTy -- We MUST save the full type now.
    let def = Ast.DefF name params (Just bodyTy) typedBody
    return $ def :<: unsafeToNoAlias VoidTy
    where
      badBody name = "The body of function" +++ codeIdent name +++ "doesn't type check"

  infer (Ast.ModF name items :@: loc) = do
    -- First, we need to put all top-level definitions into the Tcx.
    skimItemDefs `withErrMsg` ("I got stuck while skimming the contents of module" +++ codeIdent name)
    items' <- mapA infer items
    let mkPair item@(itemF :<: _) =
          ( Data.Maybe.fromMaybe (error "bad item name!") $ Ast.itemName itemF
          , Ast.modLevelItemTy item
          )
    let modTy = ModTy $ M.fromList $ map mkPair items'
    modTyNoAlias <- toNoAlias modTy
    let mod = Ast.ModF name items'
    define name modTy
    return $ mod :<: modTyNoAlias
    where
      skimItemDefs :: TyChecker ()
      skimItemDefs =
        mconcat <$> forA items (\case
          Ast.DefF name params (Just retTy) _ :@: loc -> do
            define name $ FnTy (map snd params) retTy
            return ()
          Ast.DefF name params Nothing _ :@: loc -> do
            -- Since we don't know the return type, we have to stub for now.
            define name $ FnTy (map snd params) NeverTy -- FIXME: see ./ex/rec-problem.rb
            return ()
          Ast.ModF name _ :@: loc -> do
            define name $ ModTy M.empty
            return ()
          Ast.LetConstF name _ :@: loc -> do
            define name NeverTy
            return ()
          Ast.TyDefF Ast.NotRec tyName defs :@: loc -> do
            let
              f (Ast.VrntDef name tys) = return $ M.singleton name (map getNonRecTy tys)
                where getNonRecTy = \case
                        Ast.NonRecTy ty -> ty
                        _ -> error "Can't use type `rec` in a non `rec` type!"
              f (Ast.SubTyDef name) = resolveAliasAsVrnts name
            cmpnts <- mconcat <$> mapA f defs
            let vrntTy = VrntTy cmpnts
            defineTyAlias tyName vrntTy
            return ()
          Ast.TyDefF Ast.IsRec tyName defs :@: loc -> do
            let recTyName = "?" ++ tyName
            let
              f (Ast.VrntDef name tys) = return $ M.singleton name (map g tys)
                where g = \case
                        Ast.NonRecTy ty -> ty
                        Ast.Rec -> TyVar recTyName
              f (Ast.SubTyDef name) = resolveAliasAsVrnts name
            cmpnts <- mconcat <$> mapA f defs
            let vrntTy = VrntTy cmpnts
            let recTy = RecTy recTyName vrntTy
            defineTyAlias tyName recTy
            return ()
          other -> fail $ "I can't let you put the expression" +++ code other +++ "at the top-level of a module.")

  infer (Ast.TyDefF isRec name defs :@: loc) = do
    return $ Ast.TyDefF isRec name defs :<: unsafeToNoAlias VoidTy

  -- Default case.
  infer expr = fail $ "I don't have enough information to infer the type of" +++ code expr

  check :: Ast.Expr -> Ty -> TyChecker Ast.TypedExpr

  check (Ast.BlockF seq :@: loc) ty = do
    (seq', ty) <- check seq ty
    return $ Ast.BlockF seq' :<: ty

  -- An `if` expression has two sequentially-executed sub-expressions:
  --  1. The conditional expression, and
  --  2. (One of) the branches.
  -- Therefore, the type of an `if` expression ought to be `condTy -&&> branchTy`.
  check (Ast.IfF cond yes no :@: loc) ty = do
    cond'@(_ :<: DestructureNoAlias condTy) <- check cond BoolTy `withErrMsg` badCond
    ((yes', DestructureNoAlias yesTy), (no', DestructureNoAlias noTy)) <- pairA ( check yes (condTy -&&> ty), check no (condTy -&&> ty))
    let ifExpr = Ast.IfF cond' yes' no'
    meet <- yesTy >||< noTy
    meetTy <- lift $ ((condTy -&&>) <$> meet) `toRes` RootCause "Can't join"
    meetTy' <- toNoAlias meetTy
    return $ ifExpr :<: meetTy'
    where
      badCond = "The condition of an `if` must have type" +++ code BoolTy ++ ", but this one doesn't"

  check (Ast.MatchF scrut arms :@: loc) ty = do
    scrut'@(_ :<: DestructureNoAlias scrutTy) <- infer scrut
    arms' <- forA arms $ \(refutPat, body) -> inNewScope $ do
      -- Check that all patterns have same type as scrutinee.
      refutPat' <- check refutPat scrutTy
      -- And infer the types of all the arm bodies.
      body' <- check body ty
      return (refutPat', body')
    let armBodyTys = map (\(_, (_, DestructureNoAlias ty)) -> ty) arms'
    let
      reducer :: Maybe Ty -> Ty -> TyChecker (Maybe Ty)
      reducer (Just b) a = b >||< a
      reducer Nothing _ = return Nothing
    retTyMaybe <- foldM reducer (Just ty) armBodyTys
    let retTyRes = unsafeToNoAlias . (scrutTy -&&>) <$> (retTyMaybe `toRes` RootCause msg)
          where msg = "The arms of a `match` expression must all have the same type"
    -- TODO: perform exhaustiveness/usefulness checking here.
    let arms'' = map (\(refutPat, (body, _)) -> (refutPat, body)) arms'
    lift $ (Ast.MatchF scrut' arms'' :<:) <$> retTyRes

--   -- check ctx fn@(FnExpr param body) (FnTy paramTy retTy) =
--   --   let pName = paramName param
--   --   in case check ((pName, paramTy) : ctx) body retTy of
--   --        Ok _ -> Ok (FnTy paramTy retTy)
--   --        err -> err `addError` ("Body of function `" ++ show fn ++ "` does not match expected type `" ++ show retTy ++ "`")

--   -- check ctx (Asg.Let (AnnParam var varTy) binding body) ty =
--   --   case check ctx binding varTy of
--   --        Ok _ -> check ((var, varTy):ctx) body ty
--   --        err -> err `addError` ("The variable `" ++ var ++ "` is declared as a `" ++ show varTy ++ "`, but is bound to `" ++ show binding ++ "`. This is a problem")
--   --   -- check ctx (App (FnExpr var body) binding) ty

  check (Ast.LetF pat expr :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    expr'@(_ :<: DestructureNoAlias exprTy) <- infer expr `withErrMsg` badExpr pat
    pat' <- check pat exprTy
    let letExpr = Ast.LetF pat' expr'
    return $ letExpr :<: unsafeToNoAlias (exprTy -&&> VoidTy)
    where
      notVoidMsg = "A let declaration has type" +++ code VoidTy
      badExpr pat = "The declaration of" +++ code pat +++ "needs a type annotation"

  check (Ast.AssignF name expr :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    varTy <- varLookup name `withErrMsg` problem expr name
    expr'@(_ :<: DestructureNoAlias exprTy) <- check expr varTy `withErrMsg` problem expr name
    let assign = Ast.AssignF name expr'
    return $ assign :<: unsafeToNoAlias (exprTy -&&> VoidTy)
    where
      notVoidMsg = "Assignments have type" +++ code VoidTy
      problem expr name = "The value" +++ code expr +++ "can't be assigned to variable" +++ codeIdent name

  check (Ast.LetConstF name expr :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    expr'@(_ :<: DestructureNoAlias exprTy) <- infer expr `withErrMsg` badInfer name
    define name exprTy
    let letConst = Ast.LetConstF name expr'
    return $ letConst :<: unsafeToNoAlias (exprTy -&&> VoidTy)
    where
      notVoidMsg = "A `let const` declaration has type" +++ code VoidTy
      badInfer name = "The declaration of" +++ codeIdent name +++ "needs a type annotation"

  -- For when the return type IS specified.
  check (Ast.DefF name params (Just retTy) body :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    (paramTys, body'@(_ :<: DestructureNoAlias bodyTy)) <- inNewScope $ do
      paramTys <- forA params $ uncurry define
      body' <- check body retTy `withErrMsg` badBody name retTy
      return (paramTys, body')
    if retTy == bodyTy
      then do
        let def = Ast.DefF name params (Just retTy) body'
        define name $ FnTy paramTys bodyTy
        return $ def :<: unsafeToNoAlias VoidTy
      else fail $ badBody name retTy
    where
      notVoidMsg = "A function definition expression has type" +++ code VoidTy
      badBody name retTy = "The body of function" +++ codeIdent name +++ "does not match expected type" +++ code retTy

  -- For when the return type is NOT specified.
  check (Ast.DefF name params Nothing body :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    (paramTys, body'@(_ :<: DestructureNoAlias bodyTy)) <- inNewScope $ do
      paramTys <- forA params $ uncurry define
      body' <- infer body `withErrMsg` badInfer name
      return (paramTys, body')
    let def = Ast.DefF name params Nothing body'
    define name $ FnTy paramTys bodyTy
    return $ def :<: unsafeToNoAlias VoidTy
    where
      notVoidMsg = "A function definition expression has type" +++ code VoidTy +++ "not" +++ code ty
      badInfer name = "The type of body of function" +++ codeIdent name +++ "can't be inferred"

  check (Ast.ModF name items :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    items' <- mapA infer items
    return $ Ast.ModF name items' :<: unsafeToNoAlias VoidTy
    where
      notVoidMsg = "A module definition has type" +++ code VoidTy +++ "not" +++ code ty

  -- Default case.
  check expr ty = do
    -- Switch from checking to inferring.
    expr'@(_ :<: DestructureNoAlias exprTy) <- infer expr `withErrMsg` badInfer expr
    ensureM (exprTy <: ty) (notSubtype expr exprTy ty) $ do
      return expr'
    where
      badInfer expr = "The expression" +++ code expr +++ "doesn't typecheck"
      notSubtype expr exprTy ty = "The expression" +++ code expr +++ "has type" +++ code exprTy ++ ", not" +++ code ty

checkSameType :: Ast.Seq Ast.Expr -> Ast.Seq Ast.Expr
              -> TyChecker (Res (Ast.Seq Ast.TypedExpr, Ast.Seq Ast.TypedExpr, Ty))
checkSameType e1 e2 = do
  (e1', DestructureNoAlias t1) <- infer e1
  (e2', DestructureNoAlias t2) <- infer e2
  meetRes <- t1 >||< t2
  return $ (e1', e2',) <$> (meetRes `toRes` RootCause (badMeet t1 t2))
  where
    badMeet t1 t2 = "The types" +++ code t1 +++ "and" +++ code t2 +++ "can't be joined"

astToTypedAst :: CheckType a => a -> Res (Checked a)
astToTypedAst ast = evalStateT (infer ast) initTcx
