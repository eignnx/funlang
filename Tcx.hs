{-# LANGUAGE LambdaCase #-}

module Tcx
  ( Tcx(..)
  , TyChecker(..)
  , (<:)
  , (-&&>)
  , (>||<)
  , define
  , setFnRetTy
  , varLookup
  , getFnRetTy
  , defineTyAlias
  , resolveAliasAsVrnts
  )
where

import Ty (Ty(..))
import qualified Data.Map as M
import Control.Monad.State (State, MonadState(get, put), gets)
import Control.Monad (foldM)
import Res (Res (Ok, Err), Error (RootCause), toRes)
import Utils (code, (+++), codeIdent)
import Data.List (find)

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

type TyChecker = State Tcx

(<:) :: Ty -> Ty -> TyChecker Bool
NeverTy <: t2 = return True

t1 <: t2 | t1 == t2 = return True

ValTy n1 <: ValTy n2 = return $ n1 == n2

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