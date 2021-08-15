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
import           Ast           ( Typed(HasTy) )
import           Ty            ( Ty(..) )
import           Tcx           ( TyChecker(..), (<:), (-&&>), (>||<), varLookup, define, setFnRetTy, getFnRetTy, resolveAliasAsVrnts, defineTyAlias, initTcx, NoAliasTy (getNoAlias, DestructureNoAlias), toNoAlias, unsafeToNoAlias, inNewScope, resolveAsVrnts )
import           Utils         ( (+++), code, codeIdent, indent )
import           Cata          ( RecTyped(..), At(..) )
import           Res           ( Res(..), Error (RootCause), addError, toRes, ensureM )
import qualified Intr
import qualified Data.Map      as M
import           Control.Monad ( foldM )
import           Data.Semigroup
import           Control.Monad.State
import           Control.Applicative
import           Data.Maybe    ( isJust, fromMaybe )
import Debug.Trace (trace)
import GHCi.Message (Msg(Msg))

class CheckType a where
  type Checked a :: *
  infer :: a -> TyChecker (Res (Checked a))
  check :: a -> Ty -> TyChecker (Res (Checked a))

instance CheckType Ast.Pat where
  type Checked Ast.Pat = Ast.Pat

  infer = undefined

  check (Ast.VarPat name) ty = do
    define name ty
    return $ Ok $ Ast.VarPat name

  check pat@(Ast.TuplePat ps) (TupleTy ts)
    | length ps == length ts = do
      let pairs = zip ps ts
      psRes <- sequenceA <$> mapM (uncurry check) pairs
      case psRes of
        Ok ps'  -> return $ Ok $ Ast.TuplePat ps'
        Err err -> return $ Err err
    | otherwise = return $ Err $ RootCause msg
      where msg = "The pattern" +++ code pat +++ "can't be bound to a tuple that has" +++ show (length ts) +++ "elements"

  check pat@(Ast.TuplePat ps) nonTupleTy =
    return $ Err $ RootCause msg
      where msg = "The tuple pattern" +++ code pat +++ "can't be bound to something of type" +++ code nonTupleTy

instance CheckType Ast.RefutPat where
  type Checked Ast.RefutPat = Ast.RefutPat

  infer = undefined

  check (Ast.VarRefutPat name) ty = do
    define name ty
    return $ Ok $ Ast.VarRefutPat name

  check pat@(Ast.VrntRefutPat name params) someVrnt = do
    vrntsRes <- resolveAsVrnts someVrnt
    case vrntsRes of
      Err err -> return $ Err err
      Ok vrnts ->
        case M.lookup name vrnts of
          Nothing -> return $ Err $ RootCause msg
            where msg = "I don't know what" +++ codeIdent name +++ "refers to in the pattern" +++ code pat
          Just expectedParamTys -> do
            paramsRes <- sequenceA <$> forM (zip params expectedParamTys) (\(param, expectedTy) -> do
              check param expectedTy)
            return $ Ast.VrntRefutPat name <$> paramsRes

instance CheckType (Ast.Seq Ast.Expr) where

  type Checked (Ast.Seq Ast.Expr) = (Ast.Seq Ast.TypedExpr, NoAliasTy)

  infer Ast.Empty = do
    return $ Ok (Ast.Empty, unsafeToNoAlias VoidTy)

  infer (Ast.Result e) = do
    eRes <- infer e
    case eRes of
      Ok e'@(_ :<: ty) -> do
        let result = Ast.Result e'
        return $ Ok (result, ty)
      Err err -> return $ Err err

  infer (Ast.Semi e seq) = do
    eRes <- infer e
    seqRes <- infer seq
    case (eRes, seqRes) of
      (Ok e'@(_ :<: DestructureNoAlias eTy), Ok (seq', DestructureNoAlias seqTy)) -> do
        let semi = Ast.Semi e' seq'
        let ty = unsafeToNoAlias $ eTy -&&> seqTy
        return $ Ok (semi, ty)
      _ -> return (eRes *> seqRes)

  check seq ty = do
    seqRes <- infer seq
    case seqRes of
      Ok (seq', DestructureNoAlias actual) -> do
        ensureM (actual <: ty) "" $ do
          tyRes <- toNoAlias ty
          return $ (seq',) <$> tyRes


-- | Takes a Foldable sequence of typed exprs and merges their types via `-&&>`.
--   Example:
--     operationalArgsType [a :<: IntTy, b :<: NeverTy, c :<: IntTy] == NeverTy
--     operationalArgsType [a :<: IntTy, b :<: BoolTy] == BoolTy
--     operationalArgsType [] == VoidTy
operationalArgsType :: Foldable t => t (RecTyped f) -> Ty
operationalArgsType = foldr ((-&&>) . getTy) VoidTy
  where getTy (_ :<: ty) = getNoAlias ty

checkArgs :: Ast.Expr -> [Ast.Expr] -> [Ty] -> TyChecker (Res [Ast.TypedExpr])
checkArgs fn args argTys
  | length args == length argTys =
    sequenceA <$> zipWithM check args argTys -- Check that the args have right types.
  | otherwise = return $ Err $ RootCause msg
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
             -> TyChecker (Res Ast.TypedExpr)
inferUnaryOp op argTy retTy expr = do
  exprRes <- check expr argTy
  case exprRes of
    Ok expr'@(_ :<: exprTy) -> do
      let ty = unsafeToNoAlias $ getNoAlias exprTy -&&> retTy
      return $ Ok (Ast.UnaryF op expr' :<: ty)
    Err err -> return $ Err err

-- Helper function for inferring+checking binary operators.
-- Pass in the expected type of the arguments, the expected return type, and it will do
-- the checking and inference for you.
inferBinOp :: Ast.BinOp
           -> Ty -- Expected argument type
           -> Ty -- Result type
           -> Ast.Expr
           -> Ast.Expr
           -> TyChecker (Res Ast.TypedExpr)
inferBinOp op argTy retTy e1 e2 = do
    e1Ty <- check e1 argTy
    e2Ty <- check e2 argTy
    case (e1Ty, e2Ty) of
      (Ok e1'@(_  :<: ty1), Ok e2'@(_ :<: ty2)) -> do
        let ty = unsafeToNoAlias $ getNoAlias ty1 -&&> getNoAlias ty2 -&&> retTy
        return $ Ok (Ast.BinaryF op e1' e2' :<: ty)
      (a, b) -> return (a *> b)

itemName :: Show (Ast.ExprF r) => Ast.ExprF r -> Res String
itemName expr = Ast.itemName expr `toRes` RootCause msg
  where msg = "The expression" +++ code expr +++ "can't appear in the top level"

instance CheckType Ast.Expr where

  type Checked Ast.Expr = Ast.TypedExpr

  infer (Ast.VarF name :@: loc) = do
    tyRes <- varLookup name
    case tyRes of
      Ok ty -> do
        tyNoAliasRes <- toNoAlias ty
        let var = Ast.VarF name
        return $ (var :<:) <$> tyNoAliasRes
      Err err -> return $ Err err `addError` msg
        where msg = "I can't type check the variable at" +++ show loc

  infer (Ast.LiteralF x :@: loc) =
    case x of

      Ast.Unit   -> return $ Ok $ Ast.LiteralF Ast.Unit :<: unsafeToNoAlias VoidTy

      Ast.Bool b -> return $ Ok $ Ast.LiteralF (Ast.Bool b) :<: unsafeToNoAlias BoolTy

      Ast.Int i  -> return $ Ok $ Ast.LiteralF (Ast.Int i) :<: unsafeToNoAlias IntTy

      Ast.Text t -> return $ Ok $ Ast.LiteralF (Ast.Text t) :<: unsafeToNoAlias TextTy

      Ast.Tuple exprs -> do
        exprsRes <- sequenceA <$> mapM infer exprs
        case exprsRes of
          Ok exprs' -> do
            let exprTys = map (\(_ :<: ty) -> getNoAlias ty) exprs'
            let ty = unsafeToNoAlias $ TupleTy exprTys
            return $ Ok $ Ast.LiteralF (Ast.Tuple exprs') :<: ty
          Err err -> return $ Err err

      -- To infer the type of a variant literal, just pacakge its name and the
      -- types of its arguments inside `VrntTy`.
      Ast.Vrnt name args -> do
        argsRes <- sequenceA <$> mapM infer args
        case argsRes of
          Ok args' -> do
            let argTys = map (\(_ :<: ty) -> getNoAlias ty) args'
            let ty = unsafeToNoAlias $ VrntTy $ M.singleton name argTys
            let lit = Ast.LiteralF (Ast.Vrnt name args')
            return $ Ok $ lit :<: ty
          Err err -> return $ Err err

  infer (Ast.UnaryF op@Ast.Not expr :@: loc) = inferUnaryOp op BoolTy BoolTy expr
  infer (Ast.UnaryF op@Ast.Neg expr :@: loc) = inferUnaryOp op IntTy IntTy expr
  infer e@(Ast.UnaryF (Ast.TupleProj idx) expr :@: loc) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: DestructureNoAlias (TupleTy tys))
        | fromIntegral idx < length tys -> do
          let ty = unsafeToNoAlias $ tys !! fromIntegral idx
          return $ Ok $ Ast.UnaryF (Ast.TupleProj idx) expr' :<: ty
        | otherwise -> return $ Err $ RootCause msg
          where msg = "The tuple expression" +++ code expr +++ "has only" +++ show (length tys) +++ "components, so you can't project the component at index" +++ show idx
      err -> return err

  infer (Ast.BinaryF op@(Ast.ArithOp _) e1 e2 :@: loc)          = inferBinOp op IntTy IntTy e1 e2
  infer (Ast.BinaryF op@(Ast.BoolOp  _) e1 e2 :@: loc)          = inferBinOp op BoolTy BoolTy e1 e2
  infer (Ast.BinaryF op@(Ast.RelOp _) e1 e2 :@: loc)            = inferBinOp op IntTy BoolTy e1 e2
  infer (Ast.BinaryF op@(Ast.OtherOp Ast.Concat) e1 e2 :@: loc) = inferBinOp op TextTy TextTy e1 e2

  -- This `infer` impl simply defers to `instance CheckType (Ast.Seq Ast.Expr)`.
  infer (Ast.BlockF seq :@: loc) = do
    seqRes <- infer seq
    case seqRes of
      Ok (seq', ty) -> return $ Ok $ Ast.BlockF seq' :<: ty
      Err err -> return $ Err err `addError` msg
        where msg = "I can't type check the block at" +++ show loc

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
    fnRes <- infer fn
    case fnRes of
      Ok fn'@(_ :<: DestructureNoAlias (FnTy paramTys retTy)) -> do
        argsRes <- checkArgs fn args paramTys
        case argsRes of
          Ok args' -> do
            let call = Ast.CallF fn' args'
            let ty = operationalArgsType args' -&&> retTy
            tyNoAliasRes <- toNoAlias ty
            return $ (call :<:) <$> tyNoAliasRes
          Err err -> return $ Err err `addError` msg
            where msg = "I can't type check the function call at" +++ show loc
      Ok fn'@(_ :<: DestructureNoAlias NeverTy) -> do
        -- Oops! Well, let's make the best of it. Try inferring args.
        argsRes <- sequenceA <$> mapM infer args
        let rebuild args' = Ast.CallF fn' args' :<: unsafeToNoAlias NeverTy
        return $ rebuild <$> argsRes
      Ok (_ :<: nonFnTy) -> return $ Err $ RootCause msg
        where msg = code fn +++ "is a" +++ code nonFnTy ++ ", not a function"
      err -> return err

  infer expr@(Ast.IntrinsicF place name args :@: loc) = do
    let intr = Intr.fromName name place
    let (expectedArgs, expectedRet) = Intr.sig intr
    argsRes <- checkArgs expr args expectedArgs
    case argsRes of
      Ok args' -> do
        let ty = operationalArgsType args' -&&> expectedRet
        tyNoAliasRes <- toNoAlias ty
        let intr = Ast.IntrinsicF place name args'
        return $ (intr :<:) <$> tyNoAliasRes
      Err err -> return $ Err err

  infer (Ast.LetF pat expr :@: loc) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: DestructureNoAlias exprTy) -> do
        patRes <- check pat exprTy
        case patRes of
          Ok _ -> do
            let ty = unsafeToNoAlias $ exprTy -&&> VoidTy
            let letExpr = Ast.LetF pat expr'
            return $ Ok (letExpr :<: ty)
          Err err -> return $ Err err
      err -> return $ err `addError` msg
        where msg = "The declaration of" +++ code pat +++ "doesn't type check"

  infer (Ast.AssignF name expr :@: loc) = do
    varRes <- varLookup name
    case varRes of
      Ok varTy -> do
        exprRes <- check expr varTy
        case exprRes of
          Ok expr'@(_ :<: DestructureNoAlias exprTy) -> do
            let ty = unsafeToNoAlias $ exprTy -&&> VoidTy
            let assign = Ast.AssignF name expr'
            return $ Ok (assign :<: ty)
          err -> return $ err `addError` msg
            where msg = "The value" +++ code expr +++ "can't be assigned to variable" +++ codeIdent name
      Err err -> return $ Err err `addError` msg
        where msg = "I can't assign to the undeclared variable" +++ codeIdent name

  infer (Ast.LetConstF name expr :@: loc) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: DestructureNoAlias exprTy) -> do
        define name exprTy
        let ty = unsafeToNoAlias $ exprTy -&&> VoidTy
        let letConst = Ast.LetConstF name expr'
        return $ Ok (letConst :<: ty)
      err -> return $ err `addError` msg
        where msg = "The declaration of" +++ codeIdent name +++ "doesn't type check"

  infer (Ast.RetF expr :@: loc) = do
    fnRetTy <- getFnRetTy
    exprRes <- check expr fnRetTy
    case exprRes of
      Ok expr'@(_ :<: DestructureNoAlias exprTy) -> do
        let ret = Ast.RetF expr'
        -- The `-&&>` shouldn't be necessary here, but whatever.
        let ty = unsafeToNoAlias $ exprTy -&&> NeverTy
        return $ Ok (ret :<: ty)
      err -> return err

--   -- infer ctx (FnExpr (AnnParam param paramTy) body) =
--   --   let ctx' = (param, paramTy) : ctx
--   --   in case infer ctx' body of
--   --     Ok bodyTy -> Ok $ FnTy paramTy bodyTy
--   --     err -> err
--   -- infer ctx fn@(FnExpr (Infer param) body) =
--   --   Err $ RootCause $ "I can't infer the type of the parameter `" ++ param ++ "` in the function `" ++ show fn ++ "`"

  -- To INFER the type of an if expression:
  --   1. INFER one of it's branches, then
  --   2. CHECK that the whole if expr has that type.
  -- NOTE #1: We only need to be able to INFER one (1) of the branches.
  -- NOTE #2: The traslation to Ast.TypedExpr will be done by `check`.
  infer (Ast.IfF cond yes no :@: loc) = do
    condRes <- check cond BoolTy
    bodyRes <- checkSameType yes no
    case (condRes, bodyRes) of
      (Ok cond', Ok (yes', no', bodyTy)) -> do
        let _ :<: DestructureNoAlias condTy = cond'
        let ifExpr = Ast.IfF cond' yes' no'
        tyNoAliasRes <- toNoAlias $ condTy -&&> bodyTy
        return $ (ifExpr :<:) <$> tyNoAliasRes
      (_, Err err) -> return $ condRes *> (Err err `addError` msg)
        where msg = "The two branches of this `if` expression have different types"
      _ -> return $ condRes <* bodyRes

  -- infer (Ast.MatchF scrut arms :@: loc) = do
  --   scrutRes <- infer scrut
  --   case scrutRes of
  --     Ok scrut'@(_ :<: DestructureNoAlias scrutTy) -> do
  --       armsRes <- sequenceA <$> forM arms (\(refutPat, body) -> inNewScope $ do
  --         -- Check that all patterns have same type as scrutinee.
  --         patRes <- check refutPat scrutTy
  --         -- And infer the types of all the arm bodies.
  --         bodyRes <- infer body
  --         return $ (,) <$> patRes <*> bodyRes)
  --       case armsRes of
  --         Ok arms' -> do
  --           let armBodyTys = map (\(_, (_, DestructureNoAlias ty)) -> ty) arms'
  --           let
  --             reducer :: Maybe Ty -> Ty -> TyChecker (Maybe Ty)
  --             reducer (Just b) a = b >||< a
  --             reducer Nothing _ = return Nothing
  --           retTyMaybe <- foldM reducer (Just NeverTy) armBodyTys
  --           let retTyRes = unsafeToNoAlias . (scrutTy -&&>) <$> retTyMaybe `toRes` RootCause msg
  --                where msg = "The arms of a `match` statement must all have the same type"
  --           -- TODO: perform exhaustiveness/usefulness checking here.
  --           let arms'' = map (\(refutPat, (body, _)) -> (refutPat, body)) arms'
  --           return $ (Ast.MatchF scrut' arms'' :<:) <$> retTyRes
  --         Err err -> return $ Err err

  -- A while expression does NOT return the never type. This is because
  -- usually, it does not infinitely loop. It usually loops until the
  -- condition is no longer true, then ends, yielding void.
  infer (Ast.WhileF cond body :@: loc) = do
    condRes <- check cond BoolTy
    bodyRes <- check body VoidTy -- Body ought to have type Void.
    case (condRes, bodyRes) of
        (Ok cond'@(_ :<: DestructureNoAlias condTy), Ok body'@(_ :<: DestructureNoAlias bodyTy)) -> do
          let while = Ast.WhileF cond' body'
          let ty = unsafeToNoAlias $ condTy -&&> bodyTy
          return $ Ok $ while :<: ty
        (res1, res2) -> return $ (res1 `addError` msg1) *> (res2 `addError` msg2)
          where msg1 = "The condition of this `while` loop doesn't have type" +++ code BoolTy
                msg2 = "The body of a this `while` loop doesn't have type" +++ code VoidTy

  infer (Ast.LoopF body :@: loc) = do
    bodyRes <- check body VoidTy -- Body ought to have type Void.y
    case bodyRes of
        Ok body' -> do
          let loop = Ast.LoopF body'
          return $ Ok (loop :<: unsafeToNoAlias NeverTy)
        err -> return $ err `addError` msg
          where msg = "I couldn't infer the type of the `loop` expression"

  infer (Ast.NopF :@: loc) = return $ Ok (Ast.NopF :<: unsafeToNoAlias VoidTy)

  infer (Ast.AnnF expr ty :@: loc) = do
    exprRes <- check expr ty
    tyNoAliasRes <- toNoAlias ty
    let rebuild expr' ty' = Ast.AnnF expr' ty :<: ty'
    return $ (rebuild <$> exprRes <*> tyNoAliasRes) `addError` msg
      where msg = "The expression" +++ code expr +++ "does not have type" +++ code ty

  -- For when the return type IS specified.
  infer (Ast.DefF name params (Just retTy) body :@: loc) = do
    setFnRetTy retTy -- Set fn's return type.
    (paramTys, bodyRes) <- inNewScope $ do
      paramTys <- forM params $ uncurry define
      bodyRes <- check body retTy
      return (paramTys, bodyRes)
    case bodyRes of
      Ok typedBody@(_ :<: DestructureNoAlias bodyTy) -> do
        define name $ FnTy paramTys bodyTy -- We MUST save the full type now.
        let def = Ast.DefF name params (Just retTy) typedBody
        return $ Ok (def :<: unsafeToNoAlias VoidTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The body of function" +++ codeIdent name +++ "doesn't type check"

  -- For when the return type is NOT specified.
  infer (Ast.DefF name params Nothing body :@: loc) = do
    setFnRetTy NeverTy -- We don't know the fn's return type yet! FIXME: seems bad...
    (paramTys, bodyRes) <- inNewScope $ do
      paramTys <- forM params $ uncurry define
      bodyRes <- infer body
      return (paramTys, bodyRes)
    case bodyRes of
      Ok typedBody@(_ :<: DestructureNoAlias bodyTy) -> do
        define name $ FnTy paramTys bodyTy -- We MUST save the full type now.
        let def = Ast.DefF name params (Just bodyTy) typedBody
        return $ Ok (def :<: unsafeToNoAlias VoidTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The body of function" +++ codeIdent name +++ "doesn't type check"

  infer (Ast.ModF name items :@: loc) = do
    res <- skimItemDefs -- First, we need to put all top-level definitions into the Ctx.
    case res of
      Err err -> return $ Err err `addError` msg
        where msg = "I got stuck while skimming the contents of module" +++ codeIdent name
      Ok () -> do
        itemsRes <- mapM infer items
        case sequenceA itemsRes of
          Ok items' -> do
            let mkPair item@(itemF :<: _) = (Data.Maybe.fromMaybe (error "bad item name!") $ Ast.itemName itemF, Ast.modLevelItemTy item)
            let modTy = ModTy $ M.fromList $ map mkPair items'
            modTyNoAliasRes <- toNoAlias modTy
            let mod = Ast.ModF name items'
            define name modTy
            return $ (mod :<:) <$> modTyNoAliasRes
          Err err -> return $ Err err
    where
      skimItemDefs :: TyChecker (Res ())
      skimItemDefs =
        mconcat <$> forM items (\case
          Ast.DefF name params (Just retTy) _ :@: loc -> do
            define name $ FnTy (map snd params) retTy
            return $ Ok ()
          Ast.DefF name params Nothing _ :@: loc -> do
            -- Since we don't know the return type, we have to stub for now.
            define name $ FnTy (map snd params) NeverTy -- FIXME: see ./ex/rec-problem.rb
            return $ Ok ()
          Ast.ModF name _ :@: loc -> do
            define name $ ModTy M.empty
            return $ Ok ()
          Ast.LetConstF name _ :@: loc -> do
            define name NeverTy
            return $ Ok ()
          Ast.TyDefF Ast.NotRec tyName defs :@: loc -> do
            let
              f (Ast.VrntDef name tys) = return $ Ok $ M.singleton name (map g tys)
                where g = \case
                        Ast.NonRecTy ty -> ty
                        _ -> error "Can't use type `rec` in a non `rec` type!"
              f (Ast.SubTyDef name) = resolveAliasAsVrnts name
            cmpntsUnjoined <- sequenceA <$> mapM f defs
            case mconcat <$> cmpntsUnjoined of
              Ok cmpnts -> do
                let vrntTy = VrntTy cmpnts
                defineTyAlias tyName vrntTy
                return $ Ok ()
              Err err -> return $ Err err
          Ast.TyDefF Ast.IsRec tyName defs :@: loc -> do
            let recTyName = "?" ++ tyName
            let
              f (Ast.VrntDef name tys) = return $ Ok $ M.singleton name (map g tys)
                where g = \case
                        Ast.NonRecTy ty -> ty
                        Ast.Rec -> TyVar recTyName
              f (Ast.SubTyDef name) = resolveAliasAsVrnts name
            cmpntsUnjoined <- sequenceA <$> mapM f defs
            case mconcat <$> cmpntsUnjoined of
              Ok cmpnts -> do
                let vrntTy = VrntTy cmpnts
                let recTy = RecTy recTyName vrntTy
                defineTyAlias tyName recTy
                return $ Ok ()
              Err err -> return $ Err err
          other -> return $ Err $ RootCause msg
            where msg = "I can't let you put the expression" +++ code other +++ "at the top-level of a module.")

  infer (Ast.TyDefF isRec name defs :@: loc) = do
    return $ Ok $ Ast.TyDefF isRec name defs :<: unsafeToNoAlias VoidTy

  -- Default case.
  infer expr = return $ Err $ RootCause $ msg
    where msg = "I don't have enough information to infer the type of" +++ code expr

  check :: Ast.Expr -> Ty -> TyChecker (Res Ast.TypedExpr)

  -- An `if` expression has two sequentially-executed sub-expressions:
  --  1. The conditional expression, and
  --  2. (One of) the branches.
  -- Therefore, the type of an `if` expression ought to be `condTy -&&> branchTy`.
  check (Ast.IfF cond yes no :@: loc) ty = do
    condRes <- check cond BoolTy
    yesRes <- check yes ty
    noRes <- check no ty
    case (condRes, yesRes, noRes) of
      (Ok cond'@(_ :<: DestructureNoAlias condTy), Ok (yes', _), Ok (no', _)) -> do
        let ifExpr = Ast.IfF cond' yes' no'
        tyRes <- toNoAlias $ condTy -&&> ty
        return $ (ifExpr :<:) <$> tyRes
      (condErr@(Err _), yesRes, noRes) ->
        return (condErr' <* yesRes <* noRes)
          where condErr' = condErr `addError` msg
                msg = "The condition of an `if` must have type" +++ code BoolTy ++ ", but this one doesn't"
      (_, Err yesErr, Err noErr) -> return $ Err yesErr *> Err noErr
      (_, _, Err noErr) -> return $ Err noErr
      (_, Err yesErr, _) -> return $ Err yesErr

  check (Ast.MatchF scrut arms :@: loc) ty = do
    scrutRes <- infer scrut
    case scrutRes of
      Ok scrut'@(_ :<: DestructureNoAlias scrutTy) -> do
        armsRes <- sequenceA <$> forM arms (\(refutPat, body) -> inNewScope $ do
          -- Check that all patterns have same type as scrutinee.
          patRes <- check refutPat scrutTy
          -- And infer the types of all the arm bodies.
          bodyRes <- check body ty
          return $ (,) <$> patRes <*> bodyRes)
        case armsRes of
          Ok arms' -> do
            let armBodyTys = map (\(_, (_, DestructureNoAlias ty)) -> ty) arms'
            let
              reducer :: Maybe Ty -> Ty -> TyChecker (Maybe Ty)
              reducer (Just b) a = b >||< a
              reducer Nothing _ = return Nothing
            retTyMaybe <- foldM reducer (Just NeverTy) armBodyTys
            let retTyRes = unsafeToNoAlias . (scrutTy -&&>) <$> retTyMaybe `toRes` RootCause msg
                 where msg = "The arms of a `match` statement must all have the same type"
            -- TODO: perform exhaustiveness/usefulness checking here.
            let arms'' = map (\(refutPat, (body, _)) -> (refutPat, body)) arms'
            return $ (Ast.MatchF scrut' arms'' :<:) <$> retTyRes
          Err err -> return $ Err err

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
    res <- infer expr
    case res of
      Ok expr'@(_ :<: DestructureNoAlias exprTy) -> do
        patRes <- check pat exprTy
        case patRes of
          Ok _ -> do
            let letExpr = Ast.LetF pat expr'
            return $ Ok $ letExpr :<: unsafeToNoAlias (exprTy -&&> VoidTy)
          Err err -> return $ Err err
      err -> return $ err `addError` msg
        where msg = "The declaration of" +++ code pat +++ "needs a type annotation"
    where notVoidMsg = "A let declaration has type" +++ code VoidTy

  check (Ast.AssignF name expr :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    isVoid <- ty <: VoidTy
    nameRes <- varLookup name
    case nameRes of
      Ok varTy -> do
        res <- check expr varTy
        case res of
          Ok expr'@(_ :<: DestructureNoAlias exprTy) -> do
            let assign = Ast.AssignF name expr'
            return $ Ok $ assign :<: unsafeToNoAlias (exprTy -&&> VoidTy)
          err -> return $ err `addError` msg
            where msg = "The value" +++ code expr +++ "can't be assigned to variable" +++ codeIdent name
      Err err -> return (Err err `addError` msg)
        where msg = "The value" +++ code expr +++ "can't be assigned to variable" +++ codeIdent name
    where notVoidMsg = "Assignments have type" +++ code VoidTy

  check (Ast.LetConstF name expr :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    res <- infer expr
    case res of
      Ok expr'@(_ :<: DestructureNoAlias exprTy) -> do
        define name exprTy
        let letConst = Ast.LetConstF name expr'
        return $ Ok (letConst :<: unsafeToNoAlias (exprTy -&&> VoidTy))
      err -> return $ err `addError` msg
        where msg = "The declaration of" +++ codeIdent name +++ "needs a type annotation"
    where notVoidMsg = "A `let const` declaration has type" +++ code VoidTy

  -- For when the return type IS specified.
  check (Ast.DefF name params (Just retTy) body :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    (paramTys, bodyRes) <- inNewScope $ do
      paramTys <- forM params $ uncurry define
      bodyRes <- check body retTy
      return (paramTys, bodyRes)
    case bodyRes of
      Ok body'@(_ :<: DestructureNoAlias bodyTy) | retTy == bodyTy -> do
        let def = Ast.DefF name params (Just retTy) body'
        define name $ FnTy paramTys bodyTy
        return $ Ok (def :<: unsafeToNoAlias VoidTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The body of function" +++ codeIdent name +++ "does not match expected type" +++ code retTy
    where notVoidMsg = "A function definition expression has type" +++ code VoidTy

  -- For when the return type is NOT specified.
  check (Ast.DefF name params Nothing body :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    (paramTys, bodyRes) <- inNewScope $ do
      paramTys <- forM params $ uncurry define
      bodyRes <- infer body
      return (paramTys, bodyRes)
    case bodyRes of
      Ok body'@(_ :<: DestructureNoAlias bodyTy) -> do
        let def = Ast.DefF name params Nothing body'
        define name $ FnTy paramTys bodyTy
        return $ Ok (def :<: unsafeToNoAlias VoidTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The type of body of function" +++ codeIdent name +++ "can't be inferred"
    where notVoidMsg = "A function definition expression has type" +++ code VoidTy +++ "not" +++ code ty

  check (Ast.ModF name items :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    itemsRes <- sequenceA <$> mapM infer items
    case itemsRes of
      Ok items' -> return $ Ok $ Ast.ModF name items' :<: unsafeToNoAlias VoidTy
      Err err -> return $ Err err
    where notVoidMsg = "A module definition has type" +++ code VoidTy +++ "not" +++ code ty

  -- Default case.
  check expr ty = do
    -- Switch from checking to inferring.
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: DestructureNoAlias exprTy) -> ensureM (exprTy <: ty) notSubtypeMsg $ do
        return $ Ok expr'
        where notSubtypeMsg = "The expression" +++ code expr +++ "has type" +++ code exprTy ++ ", not" +++ code ty
      err -> return $ err `addError` msg
        where msg = "The expression" +++ code expr +++ "doesn't typecheck"

checkSameType :: Ast.Seq Ast.Expr -> Ast.Seq Ast.Expr
              -> TyChecker (Res (Ast.Seq Ast.TypedExpr, Ast.Seq Ast.TypedExpr, Ty))
checkSameType e1 e2 = do
  e1Res <- infer e1
  e2Res <- infer e2
  case (e1Res, e2Res) of
    (Ok (e1', DestructureNoAlias t1), Ok (e2', DestructureNoAlias t2)) -> do
      meetRes <- t1 >||< t2
      return $ (e1', e2',) <$> (meetRes `toRes` RootCause msg)
        where msg = "The types" +++ code t1 +++ "and" +++ code t2 +++ "can't be joined"
    (Err err1, Err err2) -> return $ Err err1 *> Err err2
    (Ok _, Err err) -> return $ Err err
    (Err err, Ok _) -> return $ Err err


astToTypedAst :: CheckType a => a -> Res (Checked a)
astToTypedAst ast = evalState (infer ast) initTcx
