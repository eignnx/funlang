{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module TyCheck
  ( Res(..)
  , Error(..)
  , CheckType(..)
  , initState
  , astToTypedAst
  )
where

import qualified Ast
import           Ast           ( Typed(HasTy), RecTyped(RecHasTy) )
import           Ty            ( Ty(..), (<:) )
import qualified Intr
import qualified Data.Map      as M
import           Control.Monad ( foldM )
import           Data.Semigroup
import           Control.Monad.State
import           Control.Applicative

type Ctx = M.Map String Ty

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

data TyCheckerState = TyCheckerState { _ctx :: Ctx }
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

varLookup :: String -> TyChecker (Res Ty)
varLookup name = do
  ctx <- gets _ctx
  let reason = RootCause ("The variable `" ++ name ++ "` is not declared anywhere.")
  let res = toRes (M.lookup name ctx) reason
  return res

class CheckType a where
  type Checked a :: *
  infer :: a -> TyChecker (Res (Checked a))
  check :: a -> Ty -> TyChecker (Res (Checked a))

instance CheckType Ast.Ast where
  type Checked Ast.Ast = Ast.TypedAst

  infer :: Ast.Ast -> TyChecker (Res Ast.TypedAst)
  infer items = do
    skimItemDefs -- First, we need to put all top-level definitions into the Ctx.
    itemsRes <- mapM infer items
    case sequenceA itemsRes of
      Ok items' -> do
        let mkPair (item `HasTy` ty) = (Ast.itemName item, ty)
        let modTy = ModTy $ M.fromList $ map mkPair items'
        return $ Ok (items' `HasTy` modTy)
      Err err -> return $ Err err
    where
      skimItemDefs =
        forM_ items $ \case
          Ast.Def name params (_, Just retTy) -> 
            define name $ FnTy (map snd params) retTy
          Ast.Def name params (_, Nothing) ->
            -- Since we don't know the return type, we have to stub for now.
            define name $ FnTy (map snd params) NeverTy

  check [] (ModTy m) | m == M.empty = return $ Ok ([] `HasTy` ModTy m)
  check ast m = do
    astRes <- infer ast
    case astRes of
      Ok (ast' `HasTy` astTy) | astTy <: m -> return $ Ok (ast' `HasTy` m)
      Ok (_ `HasTy` notM) -> return $ Err $ RootCause msg
        where msg = "Module has type `" ++ show notM ++ "`, not `" ++ show m
      err -> return err

instance CheckType Ast.Item where
  type Checked Ast.Item = Ast.TypedItem

  infer :: Ast.Item -> TyChecker (Res Ast.TypedItem)

  -- For when the return type IS specified.
  infer (Ast.Def name params (body, Just retTy)) = do
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyRes <- check body retTy
    put st -- Restore old ctx
    case bodyRes of
      Ok typedBody@(_ `RecHasTy` bodyTy) -> do
        let def = Ast.Def name params (typedBody, Just retTy)
        return $ Ok (def `HasTy` (FnTy paramTys bodyTy))
      Err err -> return $ Err err

  -- For when the return type is NOT specified.
  infer (Ast.Def name params (body, Nothing)) = do
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyRes <- infer body
    put st -- Restore old ctx
    case bodyRes of
      Ok typedBody@(_ `RecHasTy` bodyTy) -> do
        let def = Ast.Def name params (typedBody, Just bodyTy)
        let ty = FnTy paramTys bodyTy
        define name ty -- We MUST save the full type now.
        return $ Ok (def `HasTy` ty)
      Err err -> return $ Err err

  check :: Ast.Item -> Ty -> TyChecker (Res Ast.TypedItem)

  -- For when the return type IS specified.
  check (Ast.Def name params (body, Just retTy)) expected@(FnTy expectedParamTys expectedRetTy) = do
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyRes <- check body expectedRetTy
    put st -- Restore old ctx
    case (paramTys == expectedParamTys, bodyRes) of
      (True, Ok body'@(_ `RecHasTy` bodyTy)) | retTy == bodyTy -> do
        let def = Ast.Def name params (body', Just retTy)
        return $ Ok (def `HasTy` expected)
      (_, Ok (_ `RecHasTy` bodyTy)) | retTy /= bodyTy -> return $ Err $ RootCause msg
        where msg = "The function `" ++ name ++ "` has type `" ++ show actualTy ++ "`, not `" ++ show expected ++ "`"
              actualTy = FnTy paramTys bodyTy
      (_, Err err) -> return $ (Err err) `addError` msg
        where msg = "The function `" ++ name ++ "` does not match expected type `" ++ show expected ++ "`"

  -- For when the return type is NOT specified.
  check (Ast.Def name params (body, Nothing)) expected@(FnTy expectedParamTys expectedRetTy) = do
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyRes <- check body expectedRetTy
    put st -- Restore old ctx
    case (paramTys == expectedParamTys, bodyRes) of
      (True, Ok body'@(_ `RecHasTy` bodyTy)) -> do
        let def = Ast.Def name params (body', Nothing)
        return $ Ok (def `HasTy` expected)
      (_, Err err) -> return $ (Err err) `addError` msg
        where msg = "The function `" ++ name ++ "` does not match expected type `" ++ show expected ++ "`"

-- Helper function for inferring+checking unary operators.
-- Pass in the expected type of the arguments, the expected return type, and it will do
-- the checking and inference for you.
inferUnaryOp :: Ast.UnaryOp
             -> Ty
             -> Ty
             -> Ast.Expr
             -> TyChecker (Res Ast.TypedExpr)
inferUnaryOp op argTy retTy expr = do
  exprRes <- check expr argTy
  case exprRes of
    Ok expr' -> return $ Ok (Ast.UnaryF op expr' `RecHasTy` retTy)
    Err err -> return $ Err err

-- Helper function for inferring+checking binary operators.
-- Pass in the expected type of the arguments, the expected return type, and it will do
-- the checking and inference for you.
inferBinOp :: Ast.BinOp
           -> Ty
           -> Ty
           -> Ast.Expr
           -> Ast.Expr
           -> TyChecker (Res Ast.TypedExpr)
inferBinOp op argTy retTy e1 e2 = do
    e1Ty <- check e1 argTy
    e2Ty <- check e2 argTy
    case (e1Ty, e2Ty) of
      (Ok e1', Ok e2') -> return $ Ok (Ast.BinaryF op e1' e2' `RecHasTy` retTy)
      (a, b) -> return (a *> b)

instance CheckType (Intr.Intrinsic, [Ast.TypedExpr]) where

  type Checked (Intr.Intrinsic, [Ast.TypedExpr]) =
    Ast.Typed (Intr.Intrinsic, [Ast.TypedExpr])

  infer (Intr.Print, args@[_ `RecHasTy` argTy]) = do
    let printableTypes = [Ty.IntTy, Ty.BoolTy, Ty.TextTy, Ty.VoidTy]
    if argTy `elem` printableTypes then do
      return $ Ok ((Intr.Print, args) `HasTy` Ty.VoidTy)
    else do
      return $ Err $ RootCause msg
        where msg = "The `print` intrinsic cannot be applied to type `" ++ show argTy ++ "`"

  infer (Intr.Here pos, []) = return $ Ok $ (Intr.Here pos, []) `HasTy` Ty.VoidTy
  infer (Intr.Exit, [])     = return $ Ok $ (Intr.Exit, []) `HasTy` Ty.NeverTy
  infer (_, _)         = return $ Err $ RootCause "You passed the wrong number of arguments to an intrinsic"

  check (_, [args]) = undefined

instance CheckType (Ast.Seq Ast.Expr) where

  type Checked (Ast.Seq Ast.Expr) = (Ast.Seq Ast.TypedExpr, Ty)

  infer (Ast.Empty) = return $ Ok (Ast.Empty, VoidTy)

  infer (Ast.Result e) = do
    eRes <- infer e
    case eRes of
      Ok e'@(_ `RecHasTy` ty) -> do
        let result = Ast.Result e'
        return $ Ok (result, ty)

  infer (Ast.Semi e seq) = do
    eRes <- infer e
    seqRes <- infer seq
    case (eRes, seqRes) of
      (Ok e'@(_ `RecHasTy` NeverTy), Ok (seq', seqTy)) -> do
        let semi = Ast.Semi e' seq'
        return $ Ok (semi, NeverTy)
      (Ok e', Ok (seq', NeverTy)) -> do
        let semi = Ast.Semi e' seq'
        return $ Ok (semi, NeverTy)
      (Ok e', Ok (seq', seqTy)) -> do
        let semi = Ast.Semi e' seq'
        return $ Ok (semi, seqTy)
      _ -> return (eRes *> seqRes)

  check = undefined

instance CheckType Ast.Expr where

  type Checked Ast.Expr = Ast.TypedExpr

  infer (Ast.Var name) = do
    tyRes <- varLookup name
    case tyRes of
      Ok ty -> do
        let var = Ast.VarF name
        return $ Ok (var `RecHasTy` ty)
      Err err -> return $ Err err

  infer (Ast.Literal x) = do
    let lit = Ast.LiteralF x
    return $ case x of
      Ast.Unit     -> Ok (lit `RecHasTy` VoidTy)
      Ast.Bool _   -> Ok (lit `RecHasTy` BoolTy)
      Ast.Int _    -> Ok (lit `RecHasTy` IntTy)
      Ast.String _ -> Ok (lit `RecHasTy` TextTy)

  infer (Ast.Unary op@Ast.Not expr) = inferUnaryOp op BoolTy BoolTy expr
  infer (Ast.Unary op@Ast.Neg expr) = inferUnaryOp op IntTy IntTy expr

  infer (Ast.Binary op@(Ast.ArithOp _) e1 e2)          = inferBinOp op IntTy IntTy e1 e2
  infer (Ast.Binary op@(Ast.BoolOp _) e1 e2)           = inferBinOp op BoolTy BoolTy e1 e2
  infer (Ast.Binary op@(Ast.RelOp Ast.Eq) e1 e2)       = return $ Err $ RootCause msg
    where msg = "I don't know how to infer the type of the `==` operator just yet. It's trickier than you'd think"
  infer (Ast.Binary op@(Ast.RelOp _) e1 e2)            = inferBinOp op IntTy BoolTy e1 e2
  infer (Ast.Binary op@(Ast.OtherOp Ast.Concat) e1 e2) = inferBinOp op TextTy TextTy e1 e2

  infer (Ast.Block seq) = do
    seqRes <- infer seq
    case seqRes of
      Ok (seq', ty) -> return $ Ok $ Ast.BlockF seq' `RecHasTy` ty
      Err err -> return $ Err err
      -- where
      --   process :: [Ast.Expr] -> TyChecker (Res Ast.TypedExpr)
      --   process [] = return $ Ok $ Ast.BlockF Ast.NotVoid [] `RecHasTy` VoidTy
      --   process (expr:exprs) = do
      --     exprRes <- infer expr
      --     exprsRes <- sequenceA <$> mapM infer exprs
      --     case (exprRes, exprsRes) of

      --       (Ok expr'@(_ `RecHasTy` NeverTy), Ok exprs') -> do
      --         let block = Ast.BlockF Ast.NotVoid (expr':exprs')
      --         return $ Ok $ block `RecHasTy` NeverTy

      --       (Ok expr', Ok exprs') -> do
      --         return $ Ok _

      --       _ -> return $ exprRes *> exprsRes
                

  infer (Ast.Call fn args) = do
    fnRes <- infer fn
    case fnRes of
      Ok fn'@(_ `RecHasTy` FnTy argTys retTy) -> do
        argsRes <- checkArgs fn args argTys
        case argsRes of
          Ok args' -> do
            let call = Ast.CallF fn' args'
            return $ Ok (call `RecHasTy` retTy)
          Err err -> return $ Err err
      Ok (_ `RecHasTy` nonFnTy) -> return $ Err $ RootCause $ msg
        where msg = "`" ++ show fn ++ "` is a `" ++ show nonFnTy ++ "`, not a function"
      err -> return err
      where
        checkArgs :: Ast.Expr -> [Ast.Expr] -> [Ty] -> TyChecker (Res [Ast.TypedExpr])
        checkArgs fn args argTys | length args == length argTys = do
          argsCheckRes <- sequenceA <$> zipWithM check args argTys -- Check that the args have right types.
          return $ argsCheckRes
        checkArgs fn args argTys = do -- Wrong number of args provided.
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
            return $ Ok (intr `RecHasTy` ty)
          Err err -> return $ Err err `addError` msg
            where msg = "The intrinsic call `" ++ show expr ++ "` has a problem"
      Err err -> return $ Err err

  infer (Ast.Let name expr) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ `RecHasTy` exprTy) -> do
        define name exprTy
        let letExpr = Ast.LetF name expr'
        return $ Ok (letExpr `RecHasTy` VoidTy)
      err -> return $ err `addError` msg
        where msg = "The declaration of `" ++ name ++ "` needs a type annotation"

  infer (Ast.Assign name expr) = do
    -- "\n   Hint: maybe you meant `let " ++ name ++ " = ...`"
    varRes <- varLookup name
    case varRes of
      Ok varTy -> do
        exprRes <- check expr varTy
        case exprRes of
          Ok expr' -> do
            let assign = Ast.AssignF name expr'
            return $ Ok (assign `RecHasTy` VoidTy)
          err -> return $ err `addError` msg
            where msg = "The value `" ++ show expr ++ "` can't be assigned to variable `" ++ name ++ "`\n   Hint: maybe you meant `let " ++ name ++ " = ...`"
      Err err -> return $ Err err `addError` msg
        where msg = "I can't assign to the undeclared variable `" ++ name ++ "`"

  infer (Ast.Ret expr) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr' -> do
        let ret = Ast.RetF expr'
        return $ Ok (ret `RecHasTy` NeverTy)
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
  infer ifExpr@(Ast.If cond yes no) = do
    yesRes <- infer yes
    noRes <- infer no
    case (yesRes, noRes) of
        (Ok (_ `RecHasTy` yesTy), _) -> check ifExpr yesTy -- Ensure yes-branch and entire expr have same type.
        (_, Ok (_ `RecHasTy` noTy)) -> check ifExpr noTy -- Ensure no-branch and entire expr have same type.
        (Err e1, Err e2) -> return $ Err (e1 <> e2) `addError` msg
          where msg = "I couldn't infer the type of the `if` expression `" ++ show ifExpr ++ "`"

  -- A while expression does NOT return the never type. This is because
  -- usually, it does not infinitely loop. It usually loops until the
  -- condition is no longer true, then ends, yielding void.
  infer (Ast.While cond body) = do
    condRes <- check cond BoolTy
    bodyRes <- check body VoidTy -- Body ought to have type Void.
    case (condRes, bodyRes) of
        (Ok cond', Ok body'@(_ `RecHasTy` bodyTy)) -> do
          let while = Ast.WhileF cond' body'
          return $ Ok (while `RecHasTy` bodyTy)
        (res1, res2) -> do
          return $ (res1 `addError` msg1) *> (res2 `addError` msg2)
          where msg1 = "The condition of this `while` loop doesn't have type `" ++ show BoolTy ++ "`"
                msg2 = "The body of a this `while` loop doesn't have type `" ++ show VoidTy ++ "`"

  infer (Ast.Loop body) = do
    bodyRes <- check body VoidTy -- Body ought to have type Void.y
    case bodyRes of
        Ok body' -> do
          let loop = Ast.LoopF body'
          return $ Ok (loop `RecHasTy` NeverTy)
        err -> return $ err `addError` msg
          where msg = "I couldn't infer the type of the `loop` expression"

  infer Ast.Nop = return $ Ok (Ast.NopF `RecHasTy` VoidTy)

  infer (Ast.Ann expr ty) = do
    res <- check expr ty
    let rebuild expr' = (Ast.AnnF expr' ty `RecHasTy` ty)
    return $ (rebuild <$> res) `addError` msg
      where msg = "The expression `" ++ show expr ++ "` does not have type `" ++ show ty ++ "`"

  -- Default case.
  infer expr = return $ Err $ RootCause $ msg
    where msg = "I don't have enough information to infer the type of `" ++ show expr ++ "`"

  check :: Ast.Expr -> Ty -> TyChecker (Res Ast.TypedExpr)

  -- Check an equality expression: `e1 == e2`
  check (Ast.Binary (Ast.RelOp Ast.Eq) e1 e2) ty | ty <: BoolTy = do
    res <- checkSameType e1 e2
    case res of
      Ok (e1', e2') -> do
        let expr = Ast.BinaryF (Ast.RelOp Ast.Eq) e1' e2'
        return $ res *> Ok (expr `RecHasTy` BoolTy)
      Err err -> return $ Err err `addError` msg
        where msg = "The arguments to the `==` operator must have the same type, but they don't"

  check (Ast.If cond yes no) ty = do
    condRes <- check cond BoolTy
    yesRes <- check yes ty
    noRes <- check no ty
    case (condRes, yesRes, noRes) of
      (Ok cond', Ok yes', Ok no') -> do
        let ifExpr = Ast.IfF cond' yes' no'
        return $ Ok (ifExpr `RecHasTy` ty)
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
        Ok expr'@(_ `RecHasTy` exprTy) -> do
          define name exprTy
          let letExpr = Ast.LetF name expr'
          return $ Ok (letExpr `RecHasTy` VoidTy)
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
          let rebuild expr' = Ast.AssignF name expr' `RecHasTy` VoidTy
          return $ rebuild <$> res
        Err err -> return (Err err `addError` msg)
          where msg = "The value `" ++ show expr ++ "` can't be assigned to variable `" ++ name ++ "`"
    else
      return $ Err $ RootCause ("Assignments have type `" ++ show VoidTy ++ "`")

  -- Default case.
  check expr ty = do
    exprRes <- infer expr
    return $ case exprRes of
      Ok expr'@(_ `RecHasTy` exprTy) | exprTy <: ty -> Ok expr'
      Ok (_ `RecHasTy` exprTy) -> Err $ RootCause msg
        where msg = "The expression `" ++ show expr ++ "` has type `" ++ show exprTy ++ "`, not `" ++ show ty ++ "`"
      err -> err `addError` msg
        where msg = "The expression\n```\n" ++ show expr ++ "\n```\ndoesn't typecheck"

checkSameType :: Ast.Expr -> Ast.Expr
              -> TyChecker (Res (Ast.TypedExpr, Ast.TypedExpr))
checkSameType e1 e2 = do
  e1Res <- infer e1 -- What happens if we CAN'T infer e1, but CAN infer e2?
  case e1Res of
    Ok e1'@(_ `RecHasTy` ty1) -> do
      e2Res <- check e2 ty1 -- Both args must be of same type.
      case e2Res of
        Ok e2' -> do
          return $ Ok (e1', e2')
        Err err -> return $ Err err
    Err err -> return $ Err err

astToTypedAst :: CheckType a => a -> Res (Checked a)
astToTypedAst ast = evalState (infer ast) initState
