{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Tcx
  ( Tcx(..)
  , TyChecker(..)
  , initTcx
  , (<:)
  , (-&&>)
  , (>||<)
  , inNewScope
  , define
  , setFnRetTy
  , varLookup
  , getFnRetTy
  , defineTyAlias
  , resolveAsVrnts
  , resolveAliasAsVrnts
  , NoAliasTy(..)
  , toNoAlias
  , unsafeToNoAlias
  )
where

import Ty (Ty(..))
import qualified Data.Map as M
import Control.Monad.State (State, MonadState(get, put), gets, modify, lift)
import Res (Res (Ok, Err), Error (RootCause), toRes)
import Utils (code, (+++), codeIdent)
import Data.List (find)
import Control.Monad (foldM, forM, join, liftM, filterM)
import Data.Monoid (All(All, getAll))
import GHC.Base (Functor)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace, traceM)

type Tcx = [TcxElem]

data TcxElem
  = VarBind String Ty
  | AliasBind String Ty
  | TyVarBind String Ty
  deriving (Show, Eq)

tcxLookupVar :: String -> Tcx -> Maybe Ty
tcxLookupVar needle = \case
  [] -> Nothing
  VarBind name ty : tcx
    | name == needle -> Just ty
    | otherwise -> tcxLookupVar needle tcx
  _ : tcx -> tcxLookupVar needle tcx

initTcx :: Tcx
initTcx = [ AliasBind "Never" NeverTy
          , AliasBind "Void" VoidTy
          , AliasBind "Bool" BoolTy
          , AliasBind "Int" IntTy
          , AliasBind "Text" TextTy
          ]

type TyChecker = State Tcx

(<:) :: Ty -> Ty -> TyChecker Bool
NeverTy <: t2 = return True

t1 <: t2 | t1 == t2 = return True

ValTy n1 <: ValTy n2 = return $ n1 == n2

AliasTy name <: ty = do
  tyRes <- resolveAlias name
  case tyRes of
    Ok resolved -> resolved <: ty
    Err err -> error $ show err

ty <: AliasTy name = do
  tyRes <- resolveAlias name
  case tyRes of
    Ok resolved -> ty <: resolved
    Err err -> error $ show err

VrntTy vs1 <: VrntTy vs2 = do
  () <- traceM $ ">>>>>>>>>>>> vrnt subtype:" +++ code (VrntTy vs1, VrntTy vs2)
  isSubmapOfM ctorSubtype vs1 vs2
  where ctorSubtype ts1 ts2 = TupleTy ts1 <: TupleTy ts2

TupleTy ts1 <: TupleTy ts2 = do
  foldM allSubtypes True (zip ts1 ts2)
  where allSubtypes True (t1, t2) = t1 <: t2
        allSubtypes False _ = return False

FnTy xs1 y1 <: FnTy xs2 y2 = do
  xRes <- foldM allSupertypes True (zip xs1 xs2)
  yRes <- y1 <: y2
  return $ xRes && yRes
  where allSupertypes True (x1, x2) = x2 <: x1
        allSupertypes False _ = return False

ModTy m1 <: ModTy m2 = return $ m2 `M.isSubmapOf` m1

r1@(RecTy x1 t1) <: r2@(RecTy x2 t2) = do
  modify $ \tcx -> TyVarBind x1 r1 : TyVarBind x2 r2 : tcx
  t1 <: t2

r@(RecTy x body) <: ty = do
  modify $ \tcx -> TyVarBind x r : tcx
  body <: ty

ty <: r@(RecTy x body) = do
  modify $ \tcx -> TyVarBind x r : tcx
  ty <: body

TyVar x <: ty = do
  repl <- gets (head . concatMap search)
  repl <: ty
  where
    search (TyVarBind x' ty) | x == x' = [ty]
    search _ = []

ty <: TyVar x = do
  repl <- gets (head . concatMap search)
  ty <: repl
  where
    search (TyVarBind x' ty) | x == x' = [ty]
    search _ = []

_ <: _ = return False

isSubmapOfM :: (Ord k, Monad m)
            => (v -> v -> m Bool)
            -> M.Map k v
            -> M.Map k v
            -> m Bool
isSubmapOfM predM m1 m2 = do
  asdf <- forM (M.toList m1) $ \(k1, v1) ->
    case M.lookup k1 m2 of
      Just v2 -> predM v1 v2
      Nothing -> return False
  return $ and asdf

-- | Short-circuits a type if the first type is `Never`.
--   For instance, in the block expression `do intr.exit[]; 1 end`, the entire
--   block ought to have type `Never -&&> Int` since `intr.exit[]` has type
--   `Never`, and it appears BEFORE the expression `1`. Therefore, the entire
--   block should have type `Never`.
(-&&>) :: Ty -> Ty -> Ty
NeverTy -&&> _ = NeverTy
_ -&&> ty = ty

-- | This operator is used to join the types of two branches. It is always the
--   case that `Never >||< ty` or `ty >||< Never` is `ty`. In general, this
--   operator returns the supertype of its two arguments.
--   NOTE: It only works if the two types are related via `<:`.
---  HMMM: Should `Int >||< Text` be `Any`? Probably not.
(>||<) :: Ty -> Ty -> TyChecker (Maybe Ty)
a >||< b = do
  aSubB <- a <: b
  bSubA <- b <: a
  case (aSubB, bSubA) of
    (True, False)  -> return $ Just b
    (False, True)  -> return $ Just a
    (True, True)   -> return $ Just a -- `a` and `b` must be equal.
    (False, False) -> do
      () <- traceM $ ">>>>>>>>>>>>>>>>>>>>>>>>> >||< failure:" +++ code (a, b)
      return Nothing

inNewScope :: TyChecker a -> TyChecker a
inNewScope prog = do
  st <- get -- Save old state
  res <- prog
  put st -- Restore old state
  return res

define :: String -> Ty -> TyChecker Ty
define name ty = do
  ctx <- get
  put $ VarBind name ty : ctx
  return ty

setFnRetTy :: Ty -> TyChecker Ty
setFnRetTy = define "#ret"

varLookup :: String -> TyChecker (Res Ty)
varLookup name = do
  ctx <- get
  let reason = RootCause ("The variable" +++ codeIdent name +++ "is not declared anywhere.")
  let res = toRes (tcxLookupVar name ctx) reason
  return res

getFnRetTy :: TyChecker Ty
getFnRetTy = do
  ctx <- get
  case tcxLookupVar "#ret" ctx of
    Just ty -> return ty
    Nothing -> error "Internal Compiler Error: couldn't find `#ret` in `ctx`!"

defineTyAlias :: String -> Ty -> TyChecker ()
defineTyAlias name ty = do
  ctx <- get
  put $ AliasBind name ty : ctx

resolveAlias :: String -> TyChecker (Res Ty)
resolveAlias name = do
  ctx <- get
  case find (searcher name) ctx of
    Just (AliasBind _ ty) -> return $ Ok ty
    _ -> return $ Err $ RootCause msg
      where msg = "I'm not aware of a type called" +++ codeIdent name
  where
    searcher n1 = \case
      AliasBind n2 ty -> n1 == n2
      _ -> False

resolveAsVrnts :: Ty -> TyChecker (Res (M.Map String [Ty]))
resolveAsVrnts = \case
  VrntTy vrnts -> return $ Ok vrnts
  AliasTy name -> resolveAliasAsVrnts name
  TyVar name -> do
    res <- resolveTyVar name
    case res of
      Ok ty -> resolveAsVrnts ty
      Err err -> return $ Err err
  r@(RecTy x body) -> do
    modify $ \tcx -> TyVarBind x r : tcx
    resolveAsVrnts body
  other -> return $ Err $ RootCause msg
    where msg = "I was expecting a variant type, but got" +++ code other

resolveAliasAsVrnts :: String -> TyChecker (Res (M.Map String [Ty]))
resolveAliasAsVrnts name = do
  res <- resolveAlias name
  return $ res >>= assumeVrnt

assumeVrnt :: Ty -> Res (M.Map String [Ty])
assumeVrnt = \case
  VrntTy vrnts -> Ok vrnts
  other -> Err $ RootCause msg
    where msg = "I was expecting a variant type, but got" +++ code other

resolveTyVar :: String -> TyChecker (Res Ty)
resolveTyVar name = do
  -- TODO: report errors from `fromJust` and `head`
  found <- gets (head . fromJust . mapM findTyVar)
  return $ Ok found
  where
    findTyVar (TyVarBind x ty) | x == name = Just ty
    findTyVar _ = Nothing

-- | A Ty that's guarunteed not to have any AliasTy's. All aliases have been resolved out
--   of the type.
newtype NoAliasTy = DestructureNoAlias { getNoAlias :: Ty }
  deriving (Eq, Ord)

instance Show NoAliasTy where
  show x = show $ getNoAlias x

unsafeToNoAlias :: Ty -> NoAliasTy
unsafeToNoAlias ty
  | trulyHasNoAliases ty = DestructureNoAlias ty
  | otherwise = error $ "`unsafeToNoAlias` is unsafe on" +++ code ty
    where trulyHasNoAliases = \case
            AliasTy _ -> False
            ValTy _ -> True
            VrntTy vrnts -> all trulyHasNoAliases $ concat $ M.elems vrnts
            TupleTy comps -> all trulyHasNoAliases comps
            FnTy params ret -> all trulyHasNoAliases params && trulyHasNoAliases ret
            ModTy m -> all trulyHasNoAliases $ M.elems m
            RecTy tyVar body -> trulyHasNoAliases body
            TyVar name -> True

toNoAlias :: Ty -> TyChecker (Res NoAliasTy)
toNoAlias = \case

  ValTy name -> return $ Ok $ unsafeToNoAlias $ ValTy name

  AliasTy name -> do
    res <- resolveAlias name
    case res of
      Ok ty -> do
        toNoAlias ty -- Make sure we recursively remove any aliases from result.
      Err err -> return $ Err err

  VrntTy vrnts -> do
    vrntsRes <- forM (M.toList vrnts) $ \(ctorName, ctorParams) -> do
      ctorParams' <- sequenceA <$> mapM toNoAlias ctorParams
      let unwrapped = map getNoAlias <$> ctorParams'
      return $ (ctorName,) <$> unwrapped
    let vrnts' = M.fromList <$> sequenceA vrntsRes
    return $ unsafeToNoAlias . VrntTy <$> vrnts'

  TupleTy ts -> do
    tsRes <- forM ts $ \ty -> do
      ty' <- toNoAlias ty
      return $ getNoAlias <$> ty'
    let ts' = sequenceA tsRes
    return $ unsafeToNoAlias . TupleTy <$> ts'

  FnTy params ret -> do
    paramsRes <- forM params $ \ty -> do
      ty' <- toNoAlias ty
      return $ getNoAlias <$> ty'
    let params' = sequenceA paramsRes
    retRes <- toNoAlias ret
    let ret' = getNoAlias <$> retRes
    let fn = FnTy <$> params' <*> ret'
    return $ unsafeToNoAlias <$> fn

  ModTy m -> do
    mRes <- forM (M.toList m) $ \(name, ty) -> do
      ty' <- toNoAlias ty
      return $ (name,) . getNoAlias <$> ty'
    let m' = M.fromList <$> sequenceA mRes
    return $ unsafeToNoAlias . ModTy <$> m'

  RecTy tyVar body -> do
    bodyRes <- toNoAlias body
    return $ unsafeToNoAlias . RecTy tyVar . getNoAlias <$> bodyRes

  TyVar name ->
    return $ Ok $ unsafeToNoAlias $ TyVar name
