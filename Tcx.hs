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
  , mapA
  , forA
  , pairA
  , withErrMsg
  )
where

import Ty (Ty(..))
import qualified Data.Map as M
import Control.Monad.State (MonadState(get, put), gets, modify, lift, StateT (runStateT), State, withState, withStateT)
import Res (Res (Ok, Err), Error (RootCause), toRes, addError)
import Utils (code, (+++), codeIdent)
import Data.List (find)
import Control.Monad (foldM, forM, join, liftM, filterM)
import Data.Monoid (All(All, getAll))
import GHC.Base (Functor)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace, traceM)
import Data.Foldable (Foldable(fold, toList))

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

type TyChecker = StateT Tcx Res

(<:) :: Ty -> Ty -> TyChecker Bool
NeverTy <: t2 = return True

t1 <: t2 | t1 == t2 = return True

ValTy n1 <: ValTy n2 = return $ n1 == n2

AliasTy name <: ty = do
  resolved <- resolveAlias name
  resolved <: ty

ty <: AliasTy name = do
  resolved <- resolveAlias name
  ty <: resolved

VrntTy vs1 <: VrntTy vs2 = do
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
      return Nothing

inNewScope :: Monad m => StateT s m a -> StateT s m a
inNewScope prog = do
  st <- get -- Save old state
  res <- prog
  put st -- Restore old state
  return res

define :: String -> Ty -> TyChecker Ty
define name ty = do
  tcx <- get
  put $ VarBind name ty : tcx
  return ty

setFnRetTy :: Ty -> TyChecker Ty
setFnRetTy = define "#ret"

varLookup :: String -> TyChecker Ty
varLookup name = do
  tcx <- get
  let res = toRes (tcxLookupVar name tcx) reason
  lift res
    where reason = RootCause ("The variable" +++ codeIdent name +++ "is not declared anywhere.")

getFnRetTy :: TyChecker Ty
getFnRetTy = do
  tcx <- get
  case tcxLookupVar "#ret" tcx of
    Just ty -> return ty
    Nothing -> error "Internal Compiler Error: couldn't find `#ret` in `tcx`!"

defineTyAlias :: String -> Ty -> TyChecker ()
defineTyAlias name ty = do
  tcx <- get
  put $ AliasBind name ty : tcx

resolveAlias :: String -> TyChecker Ty
resolveAlias name = do
  tcx <- get
  case find (searcher name) tcx of
    Just (AliasBind _ ty) -> return ty
    _ -> fail $ "I'm not aware of a type called" +++ codeIdent name
  where
    searcher n1 = \case
      AliasBind n2 ty -> n1 == n2
      _ -> False

resolveAsVrnts :: Ty -> TyChecker (M.Map String [Ty])
resolveAsVrnts = \case
  VrntTy vrnts -> return vrnts
  AliasTy name -> resolveAliasAsVrnts name
  TyVar name -> do
    ty <- resolveTyVar name
    resolveAsVrnts ty
  r@(RecTy x body) -> do
    modify $ \tcx -> TyVarBind x r : tcx
    resolveAsVrnts body
  other -> fail $ "I was expecting a variant type, but got" +++ code other

resolveAliasAsVrnts :: String -> TyChecker (M.Map String [Ty])
resolveAliasAsVrnts name = do
  ty <- resolveAlias name
  lift $ assumeVrnt ty

assumeVrnt :: Ty -> Res (M.Map String [Ty])
assumeVrnt = \case
  VrntTy vrnts -> Ok vrnts
  other -> Err $ RootCause msg
    where msg = "I was expecting a variant type, but got" +++ code other

resolveTyVar :: String -> TyChecker Ty
resolveTyVar name = do
  -- TODO: report errors from `fromJust` and `head`
  gets (head . fromJust . mapM findTyVar)
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

toNoAlias :: Ty -> TyChecker NoAliasTy
toNoAlias = \case

  ValTy name -> return $ unsafeToNoAlias $ ValTy name

  AliasTy name -> do
    ty <- resolveAlias name
    toNoAlias ty -- Make sure we recursively remove any aliases from result.

  VrntTy vrnts -> do
    vrnts <- forM (M.toList vrnts) $ \(ctorName, ctorParams) -> do
      ctorParams' <- mapM toNoAlias ctorParams
      let unwrapped = map getNoAlias ctorParams'
      return (ctorName, unwrapped)
    let vrnts' = M.fromList vrnts
    return $ unsafeToNoAlias $ VrntTy vrnts'

  TupleTy ts -> do
    ts <- forM ts $ \ty -> do
      ty' <- toNoAlias ty
      return $ getNoAlias ty'
    return $ unsafeToNoAlias $ TupleTy ts

  FnTy params ret -> do
    params <- forM params $ \ty -> do
      ty' <- toNoAlias ty
      return $ getNoAlias ty'
    retRes <- toNoAlias ret
    let ret' = getNoAlias retRes
    let fn = FnTy params ret'
    return $ unsafeToNoAlias fn

  ModTy m -> do
    mRes <- forM (M.toList m) $ \(name, ty) -> do
      ty' <- toNoAlias ty
      return (name, getNoAlias ty')
    let m' = M.fromList mRes
    return $ unsafeToNoAlias $ ModTy m'

  RecTy tyVar body -> do
    bodyRes <- toNoAlias body
    return $ unsafeToNoAlias $ RecTy tyVar $ getNoAlias bodyRes

  TyVar name ->
    return $ unsafeToNoAlias $ TyVar name

-- | The function `mapA` has same signature as `sequenceA . map`, but whereas
--   the latter would short-circuit on the first failure, `mapA` accumulates
--   failures.
mapA :: (a -> TyChecker b) -> [a] -> TyChecker [b]
mapA f as = do
  st <- get
  go st [] as
  where
    -- go :: Tcx -> [Res b] -> [a] -> TyChecker [b]
    go st bs [] =
      -- We need to run the rest of the program (beyond the `mapA` call) using
      -- the modified state.
      withStateT (const st) (lift $ sequenceA $ reverse bs)
    go st bs (a:as) = do
      case runStateT (f a) st of
        Ok (b, st') -> go st' (Ok b:bs) as
        Err err -> go st (Err err:bs) as -- Use old state here? Sure. Why not.

forA :: [a] -> (a -> TyChecker b) -> TyChecker [b]
forA as f = mapA f as

pairA :: (TyChecker a, TyChecker a) -> TyChecker (a, a)
pairA (m1, m2) = do
  asdf <- mapA id [m1, m2]
  case asdf of
    [a1, a2] -> return (a1, a2)
    _ -> error "Internal Compiler Error: You misused `pairA`!"

withErrMsg :: TyChecker a -> String -> TyChecker a
withErrMsg program msg = do
  st <- get
  case runStateT program st of
    Ok (a, st') -> withStateT (const st') $ return a
    Err err -> lift $ Err err `addError` msg