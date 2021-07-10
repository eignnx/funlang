{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module TyCheck
  ( Res(..)
  , CheckType(..)
  , initState
  , astToTypedAst
  )
where

import qualified Ast
import           Ast           ( Typed(HasTy), RecTyped(RecHasTy) )
import           Ty            ( Ty(..), (<:), neverTy, unitTy, boolTy, intTy, textTy )
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
  let reason = RootCause ("The variable `" ++ name ++ "` is not declared anywhere. Hint: maybe you meant `let " ++ name ++ " = ...`")
  let res = toRes (M.lookup name ctx) reason
  return res

class CheckType a where
  type Checked a :: *
  infer :: a -> TyChecker (Res (Checked a))
  check :: a -> Ty -> TyChecker (Res (Checked a))

instance CheckType Ast.Ast where
  type Checked Ast.Ast = Ast.TypedAst

  infer :: Ast.Ast -> TyChecker (Res Ast.TypedAst)
  infer [] = return $ Ok $ ([] `HasTy` (ModTy M.empty))
  infer (item:ast) = do
    itemRes <- infer item
    astRes  <- infer ast
    case (itemRes, astRes) of
      (Ok item'@(_ `HasTy` itemTy), Ok (ast' `HasTy` (ModTy m))) -> do
        let name = Ast.itemName item
        let m'   = M.insert name itemTy m
        return $ Ok ((item':ast') `HasTy` ModTy m')
      (Err e1, Err e2) -> return $ multiError [Err e1, Err e2]
      (_, Err e) -> return $ Err e
      (Err e, _) -> return $ Err e
      _ -> error "Unexpected thing inside `infer @Ast.Ast`!"

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
        return $ Ok (def `HasTy` (FnTy paramTys bodyTy))
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
      Ast.Unit     -> Ok (lit `RecHasTy` unitTy)
      Ast.Bool _   -> Ok (lit `RecHasTy` boolTy)
      Ast.Int _    -> Ok (lit `RecHasTy` intTy)
      Ast.String _ -> Ok (lit `RecHasTy` textTy)

  infer (Ast.Unary op@Ast.Not expr) = inferUnaryOp op boolTy boolTy expr
  infer (Ast.Unary op@Ast.Neg expr) = inferUnaryOp op intTy intTy expr

  infer (Ast.Binary op@(Ast.ArithOp _) e1 e2)          = inferBinOp op intTy intTy e1 e2
  infer (Ast.Binary op@(Ast.BoolOp _) e1 e2)           = inferBinOp op boolTy boolTy e1 e2
  infer (Ast.Binary op@(Ast.RelOp _) e1 e2)            = inferBinOp op intTy boolTy e1 e2
  infer (Ast.Binary op@(Ast.OtherOp Ast.Concat) e1 e2) = inferBinOp op textTy textTy e1 e2

  infer (Ast.Block Ast.IsVoid exprs) = do
    exprsRes <- sequenceA <$> mapM infer exprs
    case exprsRes of
      Ok exprs' -> do
        let block = Ast.BlockF Ast.IsVoid exprs'
        return $ Ok (block `RecHasTy` unitTy)
      Err err -> return $ Err err
  
  infer (Ast.Block Ast.NotVoid exprs) = do
    exprsRes <- sequenceA <$> mapM infer exprs
    case exprsRes of
      Ok [] -> do
        let block = Ast.BlockF Ast.NotVoid []
        return $ Ok (block `RecHasTy` unitTy) -- An empty block has unit type.
      Ok exprs -> do
        let block = Ast.BlockF Ast.NotVoid exprs
        let _ `RecHasTy` finalTy = last exprs
        return $ Ok (block `RecHasTy` finalTy)
      Err err -> return $ Err err

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

  infer (Ast.Intrinsic loc name args) = do
    argsRes <- sequenceA <$> mapM infer args
    case argsRes of
      Ok args' -> do
        let intr = Ast.IntrinsicF loc name args'
        let intrRetTy = unitTy -- FIXME: Cop-out for now.
        return $ Ok (intr `RecHasTy` intrRetTy)
      Err err -> return $ Err err

  infer (Ast.Let name expr) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr'@(_ `RecHasTy` exprTy) -> do
        define name exprTy
        let letExpr = Ast.LetF name expr'
        return $ Ok (letExpr `RecHasTy` unitTy)
      err -> return $ err `addError` msg
        where msg = "The declaration of `" ++ name ++ "` needs a type annotation"

  infer (Ast.Assign name expr) = do
    varRes <- varLookup name
    case varRes of
      Ok varTy -> do
        exprRes <- check expr varTy
        case exprRes of
          Ok expr' -> do
            let assign = Ast.AssignF name expr'
            return $ Ok (assign `RecHasTy` unitTy)
          err -> return $ err `addError` msg
            where msg = "The value `" ++ show expr ++ "` can't be assigned to variable `" ++ name ++ "`"
      Err err -> return $ Err err `addError` msg
        where msg = "I can't assign to the undeclared variable `" ++ name ++ "`"

  infer (Ast.Ret expr) = do
    exprRes <- infer expr
    case exprRes of
      Ok expr' -> do
        let ret = Ast.RetF expr'
        return $ Ok (ret `RecHasTy` neverTy)
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
        (e1, e2) -> return $ multiError [e1, e2] `addError` msg
          where msg = "I couldn't infer the type of the `if` expression `" ++ show ifExpr ++ "`"

  -- A while expression does NOT return the never type. This is because
  -- usually, it does not infinitely loop. It usually loops until the
  -- condition is no longer true, then ends, yielding unit (void).
  infer (Ast.While cond body) = do
    condRes <- check cond boolTy
    bodyRes <- infer body
    case (condRes, bodyRes) of
        (Ok cond', Ok body') -> do
          let while = Ast.WhileF cond' body'
          return $ Ok (while `RecHasTy` unitTy)
        (res1, res2) -> return $ multiError [res1, res2] `addError` msg
          where msg = "I couldn't infer the type of the `while` expression"

  infer Ast.Nop = return $ Ok (Ast.NopF `RecHasTy` unitTy)

  infer (Ast.Ann expr ty) = do
    res <- check expr ty
    let rebuild expr' = (Ast.AnnF expr' ty `RecHasTy` ty)
    return $ (rebuild <$> res) `addError` msg
      where msg = "Expression `" ++ show expr ++ "` does not have type `" ++ show ty ++ "`"

  -- Default case.
  infer expr = return $ Err $ RootCause $ msg
    where msg = "I don't have enough information to infer the type of `" ++ show expr ++ "`"

  check :: Ast.Expr -> Ty -> TyChecker (Res Ast.TypedExpr)

  check (Ast.If cond yes no) ty = do
    condRes <- check cond boolTy
    yesRes <- check yes ty
    noRes <- check no ty
    case (condRes, yesRes, noRes) of
      (Ok cond', Ok yes', Ok no') -> do
        let ifExpr = Ast.IfF cond' yes' no'
        return $ Ok (ifExpr `RecHasTy` ty)
      (condErr@(Err _), yesRes, noRes) ->
        return $ multiError [condErr', yesRes, noRes]
          where condErr' = condErr `addError` msg
                msg = "The condition of an `if` must have type `" ++ show boolTy ++ "`, but this one doesn't"
      (_, yesRes, noRes) -> return $ multiError [yesRes, noRes]

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
    if ty == unitTy then do
      res <- infer expr
      case res of
        Ok expr'@(_ `RecHasTy` exprTy) -> do
          define name exprTy
          let letExpr = Ast.LetF name expr'
          return $ Ok (letExpr `RecHasTy` unitTy)
        err -> return $ err `addError` msg
          where msg = "The declaration of `" ++ name ++ "` needs a type annotation"
    else
      return $ Err $ RootCause ("A let declaration has type `" ++ show unitTy ++ "`")

  check (Ast.Assign name expr) ty = do
    nameRes <- varLookup name
    case nameRes of
      Ok varTy -> do
        res <- check expr varTy
        let rebuild expr' = Ast.AssignF name expr' `RecHasTy` unitTy
        return $ rebuild <$> res
      Err err -> return (Err err `addError` msg)
        where msg = "The value `" ++ show expr ++ "` can't be assigned to variable `" ++ name ++ "`"

  -- Default case.
  check expr ty = do
    exprRes <- infer expr
    return $ case exprRes of
      Ok expr'@(_ `RecHasTy` exprTy) | exprTy <: ty -> Ok expr'
      Ok (_ `RecHasTy` exprTy) -> Err $ RootCause msg
        where msg = "Expression `" ++ show expr ++ "` has type `" ++ show exprTy ++ "`, not `" ++ show ty ++ "`"
      err -> err `addError` msg
        where msg = "The expression `" ++ show expr ++ "` doesn't typecheck"

astToTypedAst :: Ast.Ast -> Res Ast.TypedAst
astToTypedAst ast = evalState (infer ast) initState
