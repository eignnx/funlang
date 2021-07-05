{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TyCheck
  ( infer
  , check
  )
where

import qualified Ast
import           Ty            ( Ty(..), unitTy, boolTy, intTy, textTy, (<:) )
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
  show (MultiError e1 e2) = show e1 ++ "\nAlso...\n   " ++ show e2

instance Semigroup Error where
  e1 <> e2 = MultiError e1 e2

toRes :: Maybe a -> Error -> Res a
toRes (Just a) _ = Ok a
toRes Nothing reason = Err reason

addError :: Res a -> String -> Res a
addError (Ok a) _ = Ok a
addError (Err err) errMsg = Err (ResultingError errMsg err)

multiError :: [Res a] -> Res a
multiError rs = Err $ go rs
  where 
    go ((Err e):[Ok _]) = e
    go [Err e] = e
    go ((Err e):es) = MultiError e (go es)
    go ((Ok _):es) = go es

checkAllTypes :: CheckType a => [a] -> [Ty] -> TyChecker (Res [Ty])
checkAllTypes exprs tys = go (Ok []) exprs tys
  where
    go :: CheckType a => Res [Ty] -> [a] -> [Ty] -> TyChecker (Res [Ty])
    go res [] [] = return res
    go res (e:es) (t:ts) = do
      eTy <- check e t
      case eTy of
        Ok t' -> go (res <> Ok [t']) es ts
        Err e -> go (res <> Err e) es ts

data TyCheckerState = TyCheckerState { _ctx :: Ctx }
  deriving (Show)

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
  let reason = RootCause ("The variable `" ++ name ++ "` is not declared anywhere. Hint: maybe you meant `let " ++ name ++ " = ...`")
  let res = toRes (M.lookup name ctx) reason
  return res

class CheckType a where
  infer :: a -> TyChecker (Res Ty)
  check :: a -> Ty -> TyChecker (Res Ty)

instance CheckType Ast.Ast where

  infer [] = return $ Ok $ ModTy $ M.empty
  infer (item:items) = do
    itemTy <- infer item
    itemsTy <- infer items
    case (itemTy, itemsTy) of
      (Ok ty, Ok (ModTy m)) -> return $ Ok $ ModTy m'
        where m' = M.insert name ty m
              name = Ast.itemName item
      _ -> error "Unexpected thing inside `infer @Ast.Ast`!"

  check [] (ModTy m) | m == M.empty = return $ Ok $ ModTy m
  check items m = do
    itemsTy <- infer items
    case itemsTy of
      Ok itemsTy' | itemsTy' == m -> return $ Ok m
      Ok notM -> return $ Err $ RootCause msg
        where msg = "Module has type `" ++ show notM ++ "`, not `" ++ show m

instance CheckType Ast.Item where

  -- For when the return type IS specified.
  infer (Ast.Def name params (body, Just retTy)) = do
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    res <- check body retTy
    put st -- Restore old ctx
    return res

  -- For when the return type is NOT specified.
  infer (Ast.Def name params (body, Nothing)) = do
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyTy <- infer body
    put st -- Restore old ctx
    case bodyTy of
      Ok bodyTy' -> return $ Ok $ FnTy paramTys bodyTy'
      err -> return err

  -- For when the return type IS specified.
  check (Ast.Def name params (body, Just retTy)) expected@(FnTy expectedParamTys expectedRetTy) = do
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyTy <- check body expectedRetTy
    put st -- Restore old ctx
    case (paramTys == expectedParamTys, bodyTy) of
      (True, Ok bodyTy') | retTy == bodyTy' -> return $ Ok expected
      (_, Ok bodyTy') | retTy /= bodyTy' -> return $ Err $ RootCause msg
        where msg = "The function `" ++ name ++ "` has type `" ++ show actualTy ++ "`, not `" ++ show expected ++ "`"
              actualTy = FnTy paramTys bodyTy'
      (_, err) -> return $ err `addError` msg
        where msg = "The function `" ++ name ++ "` does not match expected type `" ++ show expected ++ "`"

  -- For when the return type is NOT specified.
  check (Ast.Def name params (body, Nothing)) expected@(FnTy expectedParamTys expectedRetTy) = do
    st <- get -- Save current ctx
    paramTys <- forM params $ \(param, paramTy) -> do
      define param paramTy
    bodyTy <- check body expectedRetTy
    put st -- Restore old ctx
    case (paramTys == expectedParamTys, bodyTy) of
      (True, Ok _) -> return $ Ok expected
      (False, Ok bodyTy') -> return $ Err $ RootCause msg
        where msg = "The function `" ++ name ++ "` has type `" ++ show actualTy ++ "`, not `" ++ show expected ++ "`"
              actualTy = FnTy paramTys bodyTy'
      (_, err) -> return $ err `addError` msg
        where msg = "The function `" ++ name ++ "` does not match expected type `" ++ show expected ++ "`"

-- Helper function for inferring+checking unary operators.
-- Pass in the expected type of the arguments, the expected return type, and it will do
-- the checking and inference for you.
inferUnaryOp :: Ty -> Ty -> Ast.Expr -> TyChecker (Res Ty)
inferUnaryOp argTy retTy expr = do
  exprTy <- check expr argTy
  case exprTy of
    Ok _ -> return $ Ok retTy
    err -> return err

-- Helper function for inferring+checking binary operators.
-- Pass in the expected type of the arguments, the expected return type, and it will do
-- the checking and inference for you.
inferBinOp :: Ty -> Ty -> Ast.Expr -> Ast.Expr -> TyChecker (Res Ty)
inferBinOp argTy retTy e1 e2 = do
    e1Ty <- check e1 argTy
    e2Ty <- check e2 argTy
    case (e1Ty, e2Ty) of
      (Ok _, Ok _) -> return $ Ok retTy
      (a, b) -> return (a *> b)

instance CheckType Ast.Expr where

  infer (Ast.Var name) = varLookup name

  infer (Ast.Literal x) =
    return $ case x of
      Ast.Unit     -> Ok unitTy
      Ast.Bool _   -> Ok boolTy 
      Ast.Int _    -> Ok intTy
      Ast.String _ -> Ok textTy

  infer (Ast.Unary Ast.Not expr) = inferUnaryOp boolTy boolTy expr
  infer (Ast.Unary Ast.Neg expr) = inferUnaryOp intTy intTy expr

  infer (Ast.Binary (Ast.ArithOp _) e1 e2)          = inferBinOp intTy intTy e1 e2
  infer (Ast.Binary (Ast.BoolOp _) e1 e2)           = inferBinOp boolTy boolTy e1 e2
  infer (Ast.Binary (Ast.RelOp _) e1 e2)            = inferBinOp intTy boolTy e1 e2
  infer (Ast.Binary (Ast.OtherOp Ast.Concat) e1 e2) = inferBinOp textTy textTy e1 e2

  infer (Ast.Block Ast.IsVoid exprs) = do
    case exprs of
      [] -> return $ Ok unitTy -- All `IsVoid` blocks return Unit.
      e:es -> infer e *> infer (Ast.Block Ast.IsVoid es)
  
  infer (Ast.Block Ast.NotVoid exprs) = do
    case exprs of
      [] -> return $ Ok unitTy -- An empty Block returns Unit.
      [e] -> infer e -- The base case we care about: one expression left.
      e:es -> infer e *> infer (Ast.Block Ast.NotVoid es)

  infer (Ast.Call fn args) = do
    inferred <- infer fn
    case inferred of
        Ok (FnTy argTys retTy) -> do
          res <- checkAllTypes args argTys
          return $ (res *> Ok retTy) `addError` msg
            where msg = "The function `" ++ show fn ++ "` was expecting a `" ++ show argTys ++ "` but was given `" ++ show args ++ "`. This is a problem"
        Ok nonFnTy -> return $ Err $ RootCause $ msg
          where msg = "`" ++ show fn ++ "` is a `" ++ show nonFnTy ++ "`, not a function"
        err -> return err

  infer (Ast.Intrinsic _loc name args) = return $ Ok NeverTy -- FIXME: Cop-out for now.

  infer (Ast.Let name expr) = do
    exprTy <- infer expr
    case exprTy of
      Ok exprTy' -> do
        define name exprTy'
        return $ Ok unitTy
      err -> return $ err `addError` msg
        where msg = "The declaration of `" ++ name ++ "` needs a type annotation"

  infer (Ast.Assign name expr) = do
    varTy <- varLookup name
    case varTy of
      Ok varTy' -> do
        exprTy <- check expr varTy'
        return $ exprTy `addError` msg *> Ok unitTy
          where msg = "The value `" ++ show expr ++ "` can't be assigned to variable `" ++ name ++ "`"
      err -> return $ err `addError` msg
        where msg = "I can't assign to the undeclared variable `" ++ name ++ "`"

  infer (Ast.Ret expr) = do
    exprTy <- infer expr
    case exprTy of
      Ok exprTy' -> return $ Ok NeverTy
      err -> return err

  -- infer ctx (FnExpr (AnnParam param paramTy) body) =
  --   let ctx' = (param, paramTy) : ctx
  --   in case infer ctx' body of
  --     Ok bodyTy -> Ok $ FnTy paramTy bodyTy
  --     err -> err
  -- infer ctx fn@(FnExpr (Infer param) body) =
  --   Err $ RootCause $ "I can't infer the type of the parameter `" ++ param ++ "` in the function `" ++ show fn ++ "`"

  infer ifExpr@(Ast.If cond yes no) = do
    yesCheck <- infer yes
    noCheck <- infer no
    case (yesCheck, noCheck) of
        (Ok yesTy, _) -> check ifExpr yesTy -- Ensure yes-branch and entire expr have same type.
        (_, Ok noTy) -> check ifExpr noTy -- Ensure no-branch and entire expr have same type.
        (err1, err2) -> return $ multiError [err1, err2] `addError` msg
          where msg = "I couldn't infer the type of the `if` expression"

  infer (Ast.While cond body) = do
    condTy <- check cond boolTy
    bodyTy <- infer body
    case (condTy, bodyTy) of
        (Ok _, Ok _) -> return $ Ok unitTy
        (res1, res2) -> return $ multiError [res1, res2] `addError` msg
          where msg = "I couldn't infer the type of the `while` expression"

  infer Ast.Nop = return $ Ok unitTy

  infer (Ast.Ann expr ty) = do
    res <- check expr ty
    return $ res `addError` msg
      where msg = "Expression `" ++ show expr ++ "` does not have type `" ++ show ty ++ "`"

  infer expr = return $ Err $ RootCause $ msg
    where msg = "I don't have enough information to infer the type of `" ++ show expr ++ "`"


  check (Ast.If cond yes no) ty = do
    boolCheck <- check cond boolTy
    yesCheck <- check yes ty
    noCheck <- check no ty
    case (boolCheck, yesCheck, noCheck) of
      (Ok _, Ok _, Ok _) -> return $ Ok ty
      (condErr@(Err _), yesRes, noRes) ->
        return $ multiError [condErr', yesRes, noRes]
          where condErr' = condErr `addError` msg
                msg = "The condition of an `if` must have type `" ++ show boolTy ++ "`, but this one doesn't"
      (_, yesRes, noRes) -> return $ multiError [yesRes, noRes]

  -- check ctx fn@(FnExpr param body) (FnTy paramTy retTy) =
  --   let pName = paramName param
  --   in case check ((pName, paramTy) : ctx) body retTy of
  --        Ok _ -> Ok (FnTy paramTy retTy)
  --        err -> err `addError` ("Body of function `" ++ show fn ++ "` does not match expected type `" ++ show retTy ++ "`")

  -- check ctx (Asg.Let (AnnParam var varTy) binding body) ty =
  --   case check ctx binding varTy of
  --        Ok _ -> check ((var, varTy):ctx) body ty
  --        err -> err `addError` ("The variable `" ++ var ++ "` is declared as a `" ++ show varTy ++ "`, but is bound to `" ++ show binding ++ "`. This is a problem")
  --   -- check ctx (App (FnExpr var body) binding) ty

  check (Ast.Let name expr) ty = do
    ty <- infer expr
    case ty of
      Ok varTy -> do
        define name varTy
        return $ Ok unitTy
      err -> return $ err `addError` msg
        where msg = "The declaration of `" ++ name ++ "` needs a type annotation"

  check (Ast.Assign name expr) ty = do
    ty <- varLookup name
    case ty of
      Ok varTy -> check expr varTy
      err -> return $ err `addError` msg
        where msg = "The value `" ++ show expr ++ "` can't be assigned to variable `" ++ name ++ "`"

  -- Default case.
  check expr ty = do
    exprTy <- infer expr
    return $ case exprTy of
      Ok ty' | ty' <: ty -> Ok ty'
      Ok ty' -> Err $ RootCause msg
        where msg = "Expression `" ++ show expr ++ "` has type `" ++ show ty' ++ "`, not `" ++ show ty ++ "`"
      err -> err `addError` msg
        where msg = "The expression `" ++ show expr ++ "` doesn't typecheck"