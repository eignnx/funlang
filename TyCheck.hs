{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module TyCheck
  ( Res(..)
  , Error(..)
  , CheckType(..)
  , initState
  , astToTypedAst
  )
where

import qualified Ast
import           Ast           ( Typed(HasTy), RecTyped(..) )
import           Ty            ( Ty(..), (<:), (-&&>), (>||<), downcastToFnTy )
import qualified Intr
import qualified Data.Map      as M
import           Control.Monad ( foldM )
import           Data.Semigroup
import           Control.Monad.State
import           Control.Applicative
import           Data.Maybe    ( isJust )

data Res a
  = Ok a
  | Err Error

instance Show a => Show (Res a) where
  -- show (Ok a) = "✔️: " ++ show a
  -- show (Err e) = "❗: " ++ show e
  show (Ok a) = "Ok: " ++ show a
  show (Err e) = "Err: " ++ show e

instance Semigroup a => Semigroup (Res a) where
  (Ok a) <> (Ok b) = Ok (a <> b)
  (Err a) <> (Err b) = Err (a <> b)
  (Ok a) <> (Err b) = Err b
  (Err a) <> (Ok b) = Err a

instance Monoid a => Monoid (Res a) where
  mempty = Ok $ mempty

instance Functor Res where
  fmap f (Ok a) = Ok (f a)
  fmap f (Err e) = Err e

instance Applicative Res where
  pure x = Ok x
  liftA2 f (Ok x) (Ok y) = Ok (f x y)
  liftA2 f (Err e1) (Err e2) = Err (e1 <> e2)
  liftA2 f (Err e) (Ok x) = Err e
  liftA2 f (Ok x) (Err e) = Err e

data Error
  = RootCause String
  | ResultingError String Error
  | MultiError Error Error

instance Show Error where
  show (RootCause explanation) = explanation ++ "."
  show (ResultingError extraInfo e) = extraInfo ++ " because...\n   " ++ show e
  show (MultiError e1 e2) = show e1 ++ "\n\nAlso...\n   " ++ show e2

instance Semigroup Error where
  e1 <> e2 = MultiError e1 e2

toRes :: Maybe a -> Error -> Res a
toRes (Just a) _ = Ok a
toRes Nothing reason = Err reason

addError :: Res a -> String -> Res a
addError (Ok a) _ = Ok a
addError (Err err) errMsg = Err (ResultingError errMsg err)

type Ctx = M.Map String Ty

data TyCheckerState
  = TyCheckerState { _ctx :: Ctx }
  deriving (Show)

initState :: TyCheckerState
initState = TyCheckerState { _ctx = M.empty }

type TyChecker = State TyCheckerState

define :: String -> Ty -> TyChecker Ty
define name ty = do
  ctx <- gets _ctx
  let ctx' = M.insert name ty ctx
  modify $ \st -> st { _ctx = ctx' }
  return ty

setFnRetTy :: Ty -> TyChecker Ty
setFnRetTy ty = do
  define "#ret" ty

varLookup :: String -> TyChecker (Res Ty)
varLookup name = do
  ctx <- gets _ctx
  let reason = RootCause ("The variable `" ++ name ++ "` is not declared anywhere.")
  let res = toRes (M.lookup name ctx) reason
  return res

getFnRetTy :: TyChecker Ty
getFnRetTy = do
  ctx <- gets _ctx
  case M.lookup "#ret" ctx of
    Just ty -> return ty
    Nothing -> error "Internal Compiler Error: couldn't find `#ret` in `ctx`!"

class CheckType a where
  type Checked a :: *
  infer :: a -> TyChecker (Res (Checked a))
  check :: a -> Ty -> TyChecker (Res (Checked a))

instance CheckType (Intr.Intrinsic, [Ast.TypedExpr]) where

  type Checked (Intr.Intrinsic, [Ast.TypedExpr]) =
    Ast.Typed (Intr.Intrinsic, [Ast.TypedExpr])

  infer (Intr.Print, args@[_ :<: argTy]) = do
    let printableTypes = [IntTy, BoolTy, TextTy, VoidTy]
    if any (argTy <:) printableTypes then do
      return $ Ok ((Intr.Print, args) `HasTy` (argTy -&&> VoidTy))
    else do
      return $ Err $ RootCause msg
        where msg = "The `print` intrinsic cannot be applied to type `" ++ show argTy ++ "`"

  infer (Intr.Here pos, []) = return $ Ok $ (Intr.Here pos, []) `HasTy` VoidTy
  infer (Intr.Exit, [])     = return $ Ok $ (Intr.Exit, []) `HasTy` NeverTy
  infer (_, _)              = return $ Err $ RootCause "You passed the wrong number of arguments to an intrinsic"

  check (_, [args]) = undefined

instance CheckType (Ast.Seq Ast.Expr) where

  type Checked (Ast.Seq Ast.Expr) = (Ast.Seq Ast.TypedExpr, Ty)

  infer (Ast.Empty) = return $ Ok (Ast.Empty, VoidTy)

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
itemName = \case
  Ast.DefF name _ _ _ -> Ok name
  Ast.ModF name _ -> Ok name
  Ast.LetF name _ -> Ok name
  other -> Err $ RootCause msg
    where msg = "The expression `" ++ show other ++ "` can't appear in the top level."

instance CheckType Ast.Expr where

  type Checked Ast.Expr = Ast.TypedExpr

  infer (Ast.Var name) = do
    tyRes <- varLookup name
    case tyRes of
      Ok ty -> do
        let var = Ast.VarF name
        return $ Ok (var :<: ty)
      Err err -> return $ Err err

  infer (Ast.Literal x) = do
    let lit = Ast.LiteralF x
    return $ case x of
      Ast.Unit     -> Ok (lit :<: VoidTy)
      Ast.Bool _   -> Ok (lit :<: BoolTy)
      Ast.Int _    -> Ok (lit :<: IntTy)
      Ast.String _ -> Ok (lit :<: TextTy)

  infer (Ast.Unary op@Ast.Not expr) = inferUnaryOp op BoolTy BoolTy expr
  infer (Ast.Unary op@Ast.Neg expr) = inferUnaryOp op IntTy IntTy expr

  infer (Ast.Binary op@(Ast.ArithOp _) e1 e2)          = inferBinOp op IntTy IntTy e1 e2
  infer (Ast.Binary op@(Ast.BoolOp _) e1 e2)           = inferBinOp op BoolTy BoolTy e1 e2
  infer (Ast.Binary op@(Ast.RelOp Ast.Eq) e1 e2)       = return $ Err $ RootCause msg
    where msg = "I don't know how to infer the type of the `==` operator just yet. It's trickier than you'd think"
  infer (Ast.Binary op@(Ast.RelOp Ast.Neq) e1 e2)       = return $ Err $ RootCause msg
    where msg = "I don't know how to infer the type of the `!=` operator just yet. It's trickier than you'd think"
  infer (Ast.Binary op@(Ast.RelOp _) e1 e2)            = inferBinOp op IntTy BoolTy e1 e2
  infer (Ast.Binary op@(Ast.OtherOp Ast.Concat) e1 e2) = inferBinOp op TextTy TextTy e1 e2

  -- This `infer` impl simply defers to `instance CheckType (Ast.Seq Ast.Expr)`.
  infer (Ast.Block seq) = do
    seqRes <- infer seq
    case seqRes of
      Ok (seq', ty) -> return $ Ok $ Ast.BlockF seq' :<: ty
      Err err -> return $ Err err

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
  infer (Ast.Call fn args) = do
    fnRes <- infer fn
    case fnRes of
      Ok fn'@(_ :<: fnTy) | isJust (downcastToFnTy fnTy) -> do
        let Just (paramTys, retTy) = downcastToFnTy fnTy
        argsRes <- checkArgs paramTys
        case argsRes of
          Ok args' -> do
            let call = Ast.CallF fn' args'
            let argsExeTy = foldr (-&&>) VoidTy (map (\(_ :<: ty) -> ty) args')
            return $ Ok (call :<: (argsExeTy -&&> retTy))
          Err err -> return $ Err err
      Ok fn'@(_ :<: NeverTy) -> do
        -- Oops! Well, let's make the best of it. Try inferring args.
        argsRes <- sequenceA <$> mapM infer args
        let rebuild args' = Ast.CallF fn' args' :<: NeverTy
        return $ rebuild <$> argsRes
      Ok (_ :<: nonFnTy) -> return $ Err $ RootCause $ msg
        where msg = "`" ++ show fn ++ "` is a `" ++ show nonFnTy ++ "`, not a function"
      err -> return err
    where
      checkArgs :: [Ty] -> TyChecker (Res [Ast.TypedExpr])
      checkArgs argTys | length args == length argTys = do
        sequenceA <$> zipWithM check args argTys -- Check that the args have right types.
      checkArgs argTys = do -- Wrong number of args provided.
        return $ Err $ RootCause msg
          where msg = "Oops! You passed " ++ show received ++ " arguments to `"
                    ++ show fn ++ "`, but it expects " ++ show expected
                expected = length argTys
                received = length args

  infer expr@(Ast.Intrinsic loc name args) = do
    argsRes <- sequenceA <$> mapM infer args
    case argsRes of
      Ok args' -> do
        intrRes <- infer (Intr.fromName name loc, args')
        case intrRes of
          Ok (_ `HasTy` ty) -> do
            let intr = Ast.IntrinsicF loc name args'
            return $ Ok (intr :<: ty)
          Err err -> return $ Err err `addError` msg
            where msg = "The intrinsic call `" ++ show expr ++ "` has a problem"
      Err err -> return $ Err err

  infer (Ast.Let name expr) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ :<: exprTy) -> do
        define name exprTy
        let letExpr = Ast.LetF name expr'
        return $ Ok (letExpr :<: (exprTy -&&> VoidTy))
      err -> return $ err `addError` msg
        where msg = "The declaration of `" ++ name ++ "` needs a type annotation"

  infer (Ast.Assign name expr) = do
    varRes <- varLookup name
    case varRes of
      Ok varTy -> do
        exprRes <- check expr varTy
        case exprRes of
          Ok expr'@(_ :<: exprTy) -> do
            let assign = Ast.AssignF name expr'
            return $ Ok (assign :<: (exprTy -&&> VoidTy))
          err -> return $ err `addError` msg
            where msg = "The value `" ++ show expr ++ "` can't be assigned to variable `" ++ name ++ "`"
      Err err -> return $ Err err `addError` msg
        where msg = "I can't assign to the undeclared variable `" ++ name ++ "`"

  infer (Ast.Ret expr) = do
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
  infer (Ast.If cond yes no) = do
    condRes <- check cond BoolTy
    bodyRes <- checkSameType yes no
    case (condRes, bodyRes) of
      (Ok cond', Ok (yes', no')) -> do
        let _ :<: condTy = cond'
        let _ :<: yesTy = yes'
        let _ :<: noTy = no'
        let ifExpr = Ast.IfF cond' yes' no'
        return $ Ok $ ifExpr :<: (condTy -&&> (yesTy >||< noTy))
      (_, Err err) -> return $ condRes *> (Err err `addError` msg)
        where msg = "The two branches of this `if` expression have different types"
      _ -> return $ condRes <* bodyRes

  -- A while expression does NOT return the never type. This is because
  -- usually, it does not infinitely loop. It usually loops until the
  -- condition is no longer true, then ends, yielding void.
  infer (Ast.While cond body) = do
    condRes <- check cond BoolTy
    bodyRes <- check body VoidTy -- Body ought to have type Void.
    case (condRes, bodyRes) of
        (Ok cond'@(_ :<: condTy), Ok body'@(_ :<: bodyTy)) -> do
          let while = Ast.WhileF cond' body'
          return $ Ok (while :<: (condTy -&&> bodyTy))
        (res1, res2) -> do
          return $ (res1 `addError` msg1) *> (res2 `addError` msg2)
          where msg1 = "The condition of this `while` loop doesn't have type `" ++ show BoolTy ++ "`"
                msg2 = "The body of a this `while` loop doesn't have type `" ++ show VoidTy ++ "`"

  infer (Ast.Loop body) = do
    bodyRes <- check body VoidTy -- Body ought to have type Void.y
    case bodyRes of
        Ok body' -> do
          let loop = Ast.LoopF body'
          return $ Ok (loop :<: NeverTy)
        err -> return $ err `addError` msg
          where msg = "I couldn't infer the type of the `loop` expression"

  infer Ast.Nop = return $ Ok (Ast.NopF :<: VoidTy)

  infer (Ast.Ann expr ty) = do
    res <- check expr ty
    let rebuild expr' = (Ast.AnnF expr' ty :<: ty)
    return $ (rebuild <$> res) `addError` msg
      where msg = "The expression `" ++ show expr ++ "` does not have type `" ++ show ty ++ "`"

  -- For when the return type IS specified.
  infer (Ast.Def name params (Just retTy) body) = do
    setFnRetTy retTy -- Set fn's return type.
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyRes <- check body retTy
    put st -- Restore old ctx
    case bodyRes of
      Ok typedBody@(_ :<: bodyTy) -> do
        let def = Ast.DefF name params (Just retTy) typedBody
        let ty = Fixed $ FnTy paramTys bodyTy
        return $ Ok (def :<: ty)
      Err err -> return $ Err err

  -- For when the return type is NOT specified.
  infer (Ast.Def name params Nothing body) = do
    setFnRetTy NeverTy -- We don't know the fn's return type yet! FIXME: seems bad...
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyRes <- infer body
    put st -- Restore old ctx
    case bodyRes of
      Ok typedBody@(_ :<: bodyTy) -> do
        let def = Ast.DefF name params (Just bodyTy) typedBody
        let ty = Fixed $ FnTy paramTys bodyTy
        define name ty -- We MUST save the full type now.
        return $ Ok (def :<: ty)
      Err err -> return $ Err err

  infer (Ast.Mod name items) = do
    res <- skimItemDefs -- First, we need to put all top-level definitions into the Ctx.
    case res of
      Err err -> return $ Err err `addError` ("I got stuck while skimming the contents of `mod " ++ name ++ "`")
      Ok () -> do
        itemsRes <- mapM infer items
        case sequenceA itemsRes of
          Ok items' -> do
            let mkPair (item :<: ty) = (Ast.itemName item, ty)
            let modTy = ModTy $ M.fromList $ map mkPair items'
            let mod = Ast.ModF name items'
            define name modTy
            return $ Ok (mod :<: modTy)
          Err err -> return $ Err err
    where
      skimItemDefs :: TyChecker (Res ())
      skimItemDefs =
        mconcat <$> (forM items $ \case
          Ast.Def name params (Just retTy) _ -> do
            define name $ Fixed $ FnTy (map snd params) retTy
            return $ Ok ()
          Ast.Def name params Nothing _ -> do
            -- Since we don't know the return type, we have to stub for now.
            define name $ Fixed $ FnTy (map snd params) NeverTy -- FIXME: see ./ex/rec-problem.rb
            return $ Ok ()
          Ast.Mod name _ -> do
            define name $ ModTy M.empty
            return $ Ok ()
          other -> return $ Err $ RootCause msg
            where msg = "I can't let you put the expression `" ++ show other ++ "` at the top-level of a module.")

  -- Default case.
  infer expr = return $ Err $ RootCause $ msg
    where msg = "I don't have enough information to infer the type of `" ++ show expr ++ "`"

  check :: Ast.Expr -> Ty -> TyChecker (Res Ast.TypedExpr)

  -- Check an equality/disequality expression: `e1 == e2` or `e1 != e2`
  check (Ast.Binary (Ast.RelOp op) e1 e2) ty
    | ty <: BoolTy && op `elem` [Ast.Eq, Ast.Neq] = do
      res <- checkSameType e1 e2
      case res of
        Ok (e1', e2') -> do
          let _ :<: ty1 = e1'
          let _ :<: ty2 = e2'
          let resTy = ty1 -&&> ty2 -&&> BoolTy
          let expr = Ast.BinaryF (Ast.RelOp op) e1' e2'
          return $ res *> Ok (expr :<: resTy)
        Err err -> return $ Err err `addError` msg
          where msg = "The arguments to the `==`/`!=` operator must have the same type, but they don't"

  -- An `if` expression has two sequentially-executed sub-expressions:
  --  1. The conditional expression, and
  --  2. (One of) the branches.
  -- Therefore, the type of an `if` expression ought to be `condTy -&&> branchTy`.
  check (Ast.If cond yes no) ty = do
    condRes <- check cond BoolTy
    yesRes <- check yes ty
    noRes <- check no ty
    case (condRes, yesRes, noRes) of
      (Ok cond'@(_ :<: condTy), Ok yes', Ok no') -> do
        let ifExpr = Ast.IfF cond' yes' no'
        return $ Ok (ifExpr :<: (condTy -&&> ty))
      (condErr@(Err _), yesRes, noRes) ->
        return $ (condErr' <* yesRes <* noRes)
          where condErr' = condErr `addError` msg
                msg = "The condition of an `if` must have type `" ++ show BoolTy ++ "`, but this one doesn't"
      (_, yesRes, noRes) -> return $ (yesRes *> noRes)

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

  check (Ast.Let name expr) ty =
    if ty == VoidTy then do
      res <- infer expr
      case res of
        Ok expr'@(_ :<: exprTy) -> do
          define name exprTy
          let letExpr = Ast.LetF name expr'
          return $ Ok (letExpr :<: (exprTy -&&> VoidTy))
        err -> return $ err `addError` msg
          where msg = "The declaration of `" ++ name ++ "` needs a type annotation"
    else
      return $ Err $ RootCause ("A let declaration has type `" ++ show VoidTy ++ "`")

  check (Ast.Assign name expr) ty = do
    if ty == VoidTy then do
      nameRes <- varLookup name
      case nameRes of
        Ok varTy -> do
          res <- check expr varTy
          return $ rebuild <$> res
            where rebuild expr'@(_ :<: exprTy) =
                    Ast.AssignF name expr' :<: (exprTy -&&> VoidTy)
        Err err -> return (Err err `addError` msg)
          where msg = "The value `" ++ show expr ++ "` can't be assigned to variable `" ++ name ++ "`"
    else
      return $ Err $ RootCause ("Assignments have type `" ++ show VoidTy ++ "`")

  -- For when the return type IS specified.
  check (Ast.Def name params (Just retTy) body) expected | isJust (downcastToFnTy expected) = do
    let Just (expectedParamTys, expectedRetTy) = downcastToFnTy expected
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyRes <- check body expectedRetTy
    put st -- Restore old ctx
    case (paramTys == expectedParamTys, bodyRes) of
      (True, Ok body'@(_ :<: bodyTy)) | retTy == bodyTy -> do
        let def = Ast.DefF name params (Just retTy) body'
        let actualTy = Fixed $ FnTy paramTys bodyTy
        define name actualTy
        return $ Ok (def :<: actualTy)
      (_, Ok (_ :<: bodyTy)) | retTy /= bodyTy -> return $ Err $ RootCause msg
        where msg = "The function `" ++ name ++ "` has type `" ++ show actualTy ++ "`, not `" ++ show expected ++ "`"
              actualTy = Fixed $ FnTy paramTys bodyTy
      (_, Err err) -> return $ (Err err) `addError` msg
        where msg = "The function `" ++ name ++ "` does not match expected type `" ++ show expected ++ "`"

  -- For when the return type is NOT specified.
  check (Ast.Def name params Nothing body) expected | isJust (downcastToFnTy expected) = do
    let Just (expectedParamTys, expectedRetTy) = downcastToFnTy expected
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyRes <- check body expectedRetTy
    put st -- Restore old ctx
    case (paramTys == expectedParamTys, bodyRes) of
      (True, Ok body'@(_ :<: bodyTy)) -> do
        let def = Ast.DefF name params Nothing body'
        let actualTy = Fixed $ FnTy paramTys bodyTy
        define name actualTy
        return $ Ok (def :<: actualTy)
      (_, Err err) -> return $ (Err err) `addError` msg
        where msg = "The function `" ++ name ++ "` does not match expected type `" ++ show expected ++ "`"

  check (Ast.Mod name items) ty = do
    itemsRes <- sequenceA <$> mapM infer items
    case itemsRes of
      Ok items' -> do
        let
          itemBinding :: Ast.TypedExpr -> Res (String, Ty)
          itemBinding (item :<: ty) = (,) <$> itemName item <*> pure ty
        case sequenceA $ map itemBinding items' of
          Ok bindings -> do
            let modTy = ModTy $ M.fromList $ bindings
            if modTy <: ty
              then do
                define name modTy
                return $ Ok (Ast.ModF name items' :<: modTy)
              else do
                let msg = "Module has type `" ++ show modTy ++ "`, not `" ++ show ty
                return $ Err $ RootCause msg
          Err err -> return $ Err err
      Err err -> return $ Err err

  -- Default case.
  check expr ty = do
    exprRes <- infer expr
    return $ case exprRes of
      Ok expr'@(_ :<: exprTy)
        | exprTy <: ty -> Ok expr'
        | otherwise    -> Err $ RootCause msg
          where msg = "The expression\n```\n" ++ show expr ++ "\n```\nhas type `" ++ show exprTy ++ "`, not `" ++ show ty ++ "`"
      err -> err `addError` msg
        where msg = "The expression\n```\n" ++ show expr ++ "\n```\ndoesn't typecheck"

checkSameType :: Ast.Expr -> Ast.Expr
              -> TyChecker (Res (Ast.TypedExpr, Ast.TypedExpr))
checkSameType e1 e2 = do
  e1Res <- infer e1
  case e1Res of
    Ok e1'@(_ :<: ty1) -> do
      e2Res <- check e2 ty1 -- Both args must be of same type.
      case e2Res of
        Ok e2' -> do
          return $ Ok (e1', e2')
        Err err -> return $ Err err `addError` msg
          where msg = "Both `" ++ show e1 ++ "` and `" ++ show e2 ++ "` must have the same type, but they don't"
    Err err -> do -- If we CAN'T infer e1, let's see if we can infer e2.
      e2Res <- infer e2
      case e2Res of
        Ok e2'@(_ :<: ty2) -> do
          e1CheckRes <- check e1 ty2
          case e1CheckRes of
            Ok e1' -> do
              return $ Ok (e1', e2')
            Err err -> return $ Err err
        Err err -> return $ Err err `addError` msg
          where msg = "I can't infer the type of `" ++ show e1 ++ "` or of `" ++ show e2 ++ "`"

astToTypedAst :: CheckType a => a -> Res (Checked a)
astToTypedAst ast = evalState (infer ast) initState
