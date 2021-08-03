{-# LANGUAGE LambdaCase #-}

module Intr
  (Intrinsic(..)
  , fromName
  , sig
  )
where

import qualified Text.ParserCombinators.Parsec.Pos as Parsec
import qualified Ty
import Utils ( (+++), codeIdent )

data Intrinsic
  = EqInt
  | EqBool
  | EqText
  | DbgInt
  | DbgBool
  | DbgText
  | Puts
  | Here Parsec.SourcePos
  | Exit
  deriving Show

fromName :: String -> Parsec.SourcePos -> Intrinsic
fromName name pos = case name of
  "eq-int"   -> EqInt
  "eq-bool"  -> EqBool
  "eq-text"  -> EqText
  "dbg-int"  -> DbgInt
  "dbg-bool" -> DbgBool
  "dbg-text" -> DbgText
  "puts"     -> Puts
  "here"     -> Here pos
  "exit"     -> Exit
  _          -> error $ "Unknown intrinsic" +++ codeIdent name

sig :: Intrinsic -> ([Ty.Ty], Ty.Ty)
sig = \case
  EqInt   -> ([Ty.IntTy, Ty.IntTy], Ty.BoolTy)
  EqBool  -> ([Ty.BoolTy, Ty.BoolTy], Ty.BoolTy)
  EqText  -> ([Ty.TextTy, Ty.TextTy], Ty.BoolTy)
  DbgInt  -> ([Ty.IntTy], Ty.VoidTy)
  DbgBool -> ([Ty.BoolTy], Ty.VoidTy)
  DbgText -> ([Ty.TextTy], Ty.VoidTy)
  Puts    -> ([Ty.TextTy], Ty.VoidTy)
  Here _  -> ([], Ty.TextTy)
  Exit    -> ([], Ty.NeverTy)
  
