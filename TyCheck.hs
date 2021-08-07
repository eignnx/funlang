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
import           Tcx           ( TyChecker(..), (<:), (-&&>), (>||<), varLookup, define, setFnRetTy, getFnRetTy )
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
      where msg = "The tuple pattern" +++ code pat +++ "can't be bound to something of type" ++ code nonTupleTy

instance CheckType (Ast.Seq Ast.Expr) where

  type Checked (Ast.Seq Ast.Expr) = (Ast.Seq Ast.TypedExpr, Ty)

  infer Ast.Empty = return $ Ok (Ast.Empty, VoidTy)

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
      (Ok e'@(_ :<: eTy), Ok (seq', seqTy)) -> do
        let semi = Ast.Semi e' seq'
        return $ Ok (semi, eTy -&&> seqTy)
      _ -> return (eRes *> seqRes)

  check = undefined

-- | Takes a Foldable sequence of typed exprs and merges their types via `-&&>`.
--   Example:
--     operationalArgsType [a :<: IntTy, b :<: NeverTy, c :<: IntTy] == NeverTy
--     operationalArgsType [a :<: IntTy, b :<: BoolTy] == BoolTy
--     operationalArgsType [] == VoidTy
operationalArgsType :: Foldable t => t (RecTyped f) -> Ty
operationalArgsType = foldr ((-&&>) . getTy) VoidTy
  where getTy (_ :<: ty) = ty

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
      let ty = exprTy -&&> retTy
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
        let ty = ty1 -&&> ty2 -&&> retTy
        return $ Ok (Ast.BinaryF op e1' e2' :<: ty)
      (a, b) -> return (a *> b)

itemName :: Show (Ast.ExprF r) => Ast.ExprF r -> Res String
itemName expr = Ast.itemName expr `toRes` RootCause msg
  where msg = "The expression" +++ code expr +++ "can't appear in the top level."

instance CheckType Ast.Expr where

  type Checked Ast.Expr = Ast.TypedExpr

  infer (Ast.VarF name :@: loc) = do
    tyRes <- varLookup name
    case tyRes of
      Ok ty -> do
        let var = Ast.VarF name
        return $ Ok (var :<: ty)
      Err err -> return $ Err err `addError` msg
        where msg = "I can't type check the variable at" +++ show loc

  infer (Ast.LiteralF x :@: loc) =
    case x of

      Ast.Unit     -> return $ Ok $ Ast.LiteralF Ast.Unit :<: VoidTy

      Ast.Bool b   -> return $ Ok $ Ast.LiteralF (Ast.Bool b) :<: BoolTy

      Ast.Int i    -> return $ Ok $ Ast.LiteralF (Ast.Int i) :<: IntTy

      Ast.Text t -> return $ Ok $ Ast.LiteralF (Ast.Text t) :<: TextTy

      Ast.Tuple exprs -> do
        exprsRes <- sequenceA <$> mapM infer exprs
        case exprsRes of
          Ok exprs' -> do
            let exprTys = map (\(_ :<: ty) -> ty) exprs'
            return $ Ok $ Ast.LiteralF (Ast.Tuple exprs') :<: TupleTy exprTys
          Err err -> return $ Err err

  infer (Ast.UnaryF op@Ast.Not expr :@: loc) = inferUnaryOp op BoolTy BoolTy expr
  infer (Ast.UnaryF op@Ast.Neg expr :@: loc) = inferUnaryOp op IntTy IntTy expr
  infer e@(Ast.UnaryF (Ast.TupleProj idx) expr :@: loc) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: TupleTy tys)
        | fromIntegral idx < length tys -> do
          let ty = tys !! fromIntegral idx
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
      Ok fn'@(_ :<: FnTy paramTys retTy) -> do
        argsRes <- checkArgs fn args paramTys
        case argsRes of
          Ok args' -> do
            let call = Ast.CallF fn' args'
            let ty = operationalArgsType args' -&&> retTy
            return $ Ok (call :<: ty)
          Err err -> return $ Err err `addError` msg
            where msg = "I can't type check the function call at" +++ show loc
      Ok fn'@(_ :<: NeverTy) -> do
        -- Oops! Well, let's make the best of it. Try inferring args.
        argsRes <- sequenceA <$> mapM infer args
        let rebuild args' = Ast.CallF fn' args' :<: NeverTy
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
        let intr = Ast.IntrinsicF place name args'
        return $ Ok $ intr :<: ty
      Err err -> return $ Err err

  infer (Ast.LetF pat expr :@: loc) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: exprTy) -> do
        patRes <- check pat exprTy
        case patRes of
          Ok _ -> do
            let letExpr = Ast.LetF pat expr'
            return $ Ok (letExpr :<: (exprTy -&&> VoidTy))
          Err err -> return $ Err err
      err -> return $ err `addError` msg
        where msg = "The declaration of" +++ code pat +++ "doesn't type check"

  infer (Ast.AssignF name expr :@: loc) = do
    varRes <- varLookup name
    case varRes of
      Ok varTy -> do
        exprRes <- check expr varTy
        case exprRes of
          Ok expr'@(_ :<: exprTy) -> do
            let assign = Ast.AssignF name expr'
            return $ Ok (assign :<: (exprTy -&&> VoidTy))
          err -> return $ err `addError` msg
            where msg = "The value" +++ code expr +++ "can't be assigned to variable" +++ codeIdent name
      Err err -> return $ Err err `addError` msg
        where msg = "I can't assign to the undeclared variable" +++ codeIdent name

  infer (Ast.LetConstF name expr :@: loc) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: exprTy) -> do
        define name exprTy
        let letConst = Ast.LetConstF name expr'
        return $ Ok (letConst :<: (exprTy -&&> VoidTy))
      err -> return $ err `addError` msg
        where msg = "The declaration of" +++ codeIdent name +++ "doesn't type check"

  infer (Ast.RetF expr :@: loc) = do
    fnRetTy <- getFnRetTy
    exprRes <- check expr fnRetTy
    case exprRes of
      Ok expr'@(_ :<: exprTy) -> do
        let ret = Ast.RetF expr'
        -- The `-&&>` shouldn't be necessary here, but whatever.
        return $ Ok (ret :<: (exprTy -&&> NeverTy))
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
        let _ :<: condTy = cond'
        let ifExpr = Ast.IfF cond' yes' no'
        return $ Ok $ ifExpr :<: (condTy -&&> bodyTy)
      (_, Err err) -> return $ condRes *> (Err err `addError` msg)
        where msg = "The two branches of this `if` expression have different types"
      _ -> return $ condRes <* bodyRes

  -- A while expression does NOT return the never type. This is because
  -- usually, it does not infinitely loop. It usually loops until the
  -- condition is no longer true, then ends, yielding void.
  infer (Ast.WhileF cond body :@: loc) = do
    condRes <- check cond BoolTy
    bodyRes <- check body VoidTy -- Body ought to have type Void.
    case (condRes, bodyRes) of
        (Ok cond'@(_ :<: condTy), Ok body'@(_ :<: bodyTy)) -> do
          let while = Ast.WhileF cond' body'
          return $ Ok (while :<: (condTy -&&> bodyTy))
        (res1, res2) -> return $ (res1 `addError` msg1) *> (res2 `addError` msg2)
          where msg1 = "The condition of this `while` loop doesn't have type" +++ code BoolTy
                msg2 = "The body of a this `while` loop doesn't have type" +++ code VoidTy

  infer (Ast.LoopF body :@: loc) = do
    bodyRes <- check body VoidTy -- Body ought to have type Void.y
    case bodyRes of
        Ok body' -> do
          let loop = Ast.LoopF body'
          return $ Ok (loop :<: NeverTy)
        err -> return $ err `addError` msg
          where msg = "I couldn't infer the type of the `loop` expression"

  infer (Ast.NopF :@: loc) = return $ Ok (Ast.NopF :<: VoidTy)

  infer (Ast.AnnF expr ty :@: loc) = do
    res <- check expr ty
    let rebuild expr' = Ast.AnnF expr' ty :<: ty
    return $ (rebuild <$> res) `addError` msg
      where msg = "The expression" +++ code expr +++ "does not have type" +++ code ty

  -- For when the return type IS specified.
  infer (Ast.DefF name params (Just retTy) body :@: loc) = do
    setFnRetTy retTy -- Set fn's return type.
    st <- get -- Save current ctx
    paramTys <- forM params $ uncurry define
    bodyRes <- check body retTy
    put st -- Restore old ctx
    case bodyRes of
      Ok typedBody@(_ :<: bodyTy) -> do
        define name $ FnTy paramTys bodyTy -- We MUST save the full type now.
        let def = Ast.DefF name params (Just retTy) typedBody
        return $ Ok (def :<: VoidTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The body of function" +++ codeIdent name +++ "doesn't type check"

  -- For when the return type is NOT specified.
  infer (Ast.DefF name params Nothing body :@: loc) = do
    setFnRetTy NeverTy -- We don't know the fn's return type yet! FIXME: seems bad...
    st <- get -- Save current ctx
    paramTys <- forM params $ uncurry define
    bodyRes <- infer body
    put st -- Restore old ctx
    case bodyRes of
      Ok typedBody@(_ :<: bodyTy) -> do
        define name $ FnTy paramTys bodyTy -- We MUST save the full type now.
        let def = Ast.DefF name params (Just bodyTy) typedBody
        return $ Ok (def :<: VoidTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The body of function" +++ codeIdent name +++ "doesn't type check"

  infer (Ast.ModF name items :@: loc) = do
    res <- skimItemDefs -- First, we need to put all top-level definitions into the Ctx.
    case res of
      Err err -> return $ Err err `addError` ("I got stuck while skimming the contents of module" +++ codeIdent name)
      Ok () -> do
        itemsRes <- mapM infer items
        case sequenceA itemsRes of
          Ok items' -> do
            let mkPair item@(itemF :<: _) = (Data.Maybe.fromMaybe (error "bad item name!") $ Ast.itemName itemF, Ast.modLevelItemTy item)
            let modTy = ModTy $ M.fromList $ map mkPair items'
            let mod = Ast.ModF name items'
            define name modTy
            return $ Ok (mod :<: modTy)
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
          other -> return $ Err $ RootCause msg
            where msg = "I can't let you put the expression" +++ code other +++ "at the top-level of a module.")

  -- -- Default case.
  -- infer expr = return $ Err $ RootCause $ msg
  --   where msg = "I don't have enough information to infer the type of" +++ code expr

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
      (Ok cond'@(_ :<: condTy), Ok (yes', _), Ok (no', _)) -> do
        let ifExpr = Ast.IfF cond' yes' no'
        return $ Ok (ifExpr :<: (condTy -&&> ty))
      (condErr@(Err _), yesRes, noRes) ->
        return (condErr' <* yesRes <* noRes)
          where condErr' = condErr `addError` msg
                msg = "The condition of an `if` must have type" +++ code BoolTy ++ ", but this one doesn't"
      (_, Err yesErr, Err noErr) -> return $ Err yesErr *> Err noErr
      (_, _, Err noErr) -> return $ Err noErr
      (_, Err yesErr, _) -> return $ Err yesErr

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
      Ok expr'@(_ :<: exprTy) -> do
        patRes <- check pat exprTy
        case patRes of
          Ok _ -> do
            let letExpr = Ast.LetF pat expr'
            return $ Ok $ letExpr :<: (exprTy -&&> VoidTy)
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
          Ok expr'@(_ :<: exprTy) -> do
            let assign = Ast.AssignF name expr'
            return $ Ok $ assign :<: (exprTy -&&> VoidTy)
          err -> return $ err `addError` msg
            where msg = "The value" +++ code expr +++ "can't be assigned to variable" +++ codeIdent name
      Err err -> return (Err err `addError` msg)
        where msg = "The value" +++ code expr +++ "can't be assigned to variable" +++ codeIdent name
    where notVoidMsg = "Assignments have type" +++ code VoidTy

  check (Ast.LetConstF name expr :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    res <- infer expr
    case res of
      Ok expr'@(_ :<: exprTy) -> do
        define name exprTy
        let letConst = Ast.LetConstF name expr'
        return $ Ok (letConst :<: (exprTy -&&> VoidTy))
      err -> return $ err `addError` msg
        where msg = "The declaration of" +++ codeIdent name +++ "needs a type annotation"
    where notVoidMsg = "A `let const` declaration has type" +++ code VoidTy

  -- For when the return type IS specified.
  check (Ast.DefF name params (Just retTy) body :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    st <- get -- Save current ctx
    paramTys <- forM params $ uncurry define
    bodyRes <- check body retTy
    put st -- Restore old ctx
    case bodyRes of
      Ok body'@(_ :<: bodyTy) | retTy == bodyTy -> do
        let def = Ast.DefF name params (Just retTy) body'
        define name $ FnTy paramTys bodyTy
        return $ Ok (def :<: VoidTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The body of function" +++ codeIdent name +++ "does not match expected type" +++ code retTy
    where notVoidMsg = "A function definition expression has type" +++ code VoidTy

  -- For when the return type is NOT specified.
  check (Ast.DefF name params Nothing body :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    st <- get -- Save current ctx
    paramTys <- forM params $ uncurry define
    bodyRes <- infer body
    put st -- Restore old ctx
    case bodyRes of
      Ok body'@(_ :<: bodyTy) -> do
        let def = Ast.DefF name params Nothing body'
        define name $ FnTy paramTys bodyTy
        return $ Ok (def :<: VoidTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The type of body of function" +++ codeIdent name +++ "can't be inferred"
    where notVoidMsg = "A function definition expression has type" +++ code VoidTy +++ "not" +++ code ty

  check (Ast.ModF name items :@: loc) ty = ensureM (ty <: VoidTy) notVoidMsg $ do
    itemsRes <- sequenceA <$> mapM infer items
    case itemsRes of
      Ok items' -> return $ Ok $ Ast.ModF name items' :<: VoidTy
      Err err -> return $ Err err
    where notVoidMsg = "A module definition has type" +++ code VoidTy +++ "not" +++ code ty

  -- Default case.
  check expr ty = do
    -- Switch from checking to inferring.
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: exprTy) -> ensureM (exprTy <: ty) notSubtypeMsg $ do
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
    (Ok (e1', t1), Ok (e2', t2)) -> do
      meetRes <- t1 >||< t2
      return $ (e1', e2',) <$> (meetRes `toRes` RootCause msg)
        where msg = "The types" +++ code t1 +++ "and" +++ code t2 +++ "can't be joined"
    (Err err1, Err err2) -> return $ Err err1 *> Err err2
    (Ok _, Err err) -> return $ Err err
    (Err err, Ok _) -> return $ Err err


astToTypedAst :: CheckType a => a -> Res (Checked a)
astToTypedAst ast = evalState (infer ast) []
