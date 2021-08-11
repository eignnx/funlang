{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Tcx
  ( Tcx(..)
  , TyChecker(..)
  , initTcx
  , (<:)
  , (-&&>)
  , (>||<)
  , define
  , setFnRetTy
  , varLookup
  , getFnRetTy
  , defineTyAlias
  , resolveAliasAsVrnts
  , NoAliasTy(..)
  , toNoAlias
  , unsafeToNoAlias
  )
where

import Ty (Ty(..))
import qualified Data.Map as M
import Control.Monad.State (State, MonadState(get, put), gets)
import Res (Res (Ok, Err), Error (RootCause), toRes)
import Utils (code, (+++), codeIdent)
import Data.List (find)
import Control.Monad (foldM, forM)
import Data.Monoid (All(All, getAll))
import GHC.Base (Functor)

type Tcx = [TcxElem]

data TcxElem
  = VarBind String Ty
  | AliasBind String Ty
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
    Err err -> return $ error $ show err

ty <: AliasTy name = do
  tyRes <- resolveAlias name
  case tyRes of
    Ok resolved -> ty <: resolved
    Err err -> return $ error $ show err

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
    (False, False) -> return Nothing

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

resolveAliasAsVrnts :: String -> TyChecker (Res (M.Map String [Ty]))
resolveAliasAsVrnts name = do
  res <- resolveAlias name
  case res of
    Ok (VrntTy vrnts) -> return $ Ok vrnts
    Ok other -> return $ Err $ RootCause msg
      where msg = "The type" +++ codeIdent name +++ "refers to a" +++ code other +++ "not a variant type"
    Err err -> return $ Err err

-- | A Ty that's guarunteed not to have any AliasTy's. All aliases have been resolved out
--   of the type.
newtype NoAliasTy = DestructureNoAlias { getNoAlias :: Ty }
  deriving (Eq, Ord)

instance Show NoAliasTy where
  show x = show $ getNoAlias x

unsafeToNoAlias :: Ty -> NoAliasTy
unsafeToNoAlias = DestructureNoAlias

toNoAlias :: Ty -> TyChecker (Res NoAliasTy)
toNoAlias = \case

  ValTy name -> return $ Ok $ DestructureNoAlias $ ValTy name

  AliasTy name -> do
    res <- resolveAlias name
    case res of
      Ok ty -> do
        return $ Ok $ DestructureNoAlias ty
      Err err -> return $ Err err

  VrntTy vrnts -> do
    vrntsRes <- forM (M.toList vrnts) $ \(ctorName, ctorParams) -> do
      ctorParams' <- sequenceA <$> mapM toNoAlias ctorParams
      let unwrapped = map getNoAlias <$> ctorParams'
      return $ (ctorName,) <$> unwrapped
    let vrnts' = M.fromList <$> sequenceA vrntsRes
    return $ DestructureNoAlias . VrntTy <$> vrnts'

  TupleTy ts -> do
    tsRes <- forM ts $ \ty -> do
      ty' <- toNoAlias ty
      return $ getNoAlias <$> ty'
    let ts' = sequenceA tsRes
    return $ DestructureNoAlias . TupleTy <$> ts'

  FnTy params ret -> do
    paramsRes <- forM params $ \ty -> do
      ty' <- toNoAlias ty
      return $ getNoAlias <$> ty'
    let params' = sequenceA paramsRes
    retRes <- toNoAlias ret
    let ret' = getNoAlias <$> retRes
    let fn = FnTy <$> params' <*> ret'
    return $ DestructureNoAlias <$> fn

  ModTy m -> do
    mRes <- forM (M.toList m) $ \(name, ty) -> do
      ty' <- toNoAlias ty
      return $ (name,) . getNoAlias <$> ty'
    let m' = M.fromList <$> sequenceA mRes
    return $ DestructureNoAlias . ModTy <$> m'
