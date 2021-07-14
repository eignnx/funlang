{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Intr
  (Intrinsic(..)
  , fromName
  )
where

import qualified Text.ParserCombinators.Parsec.Pos as Parsec

data Intrinsic
  = Print
  | Here Parsec.SourcePos
  | Exit
  deriving Show

fromName :: String -> Parsec.SourcePos -> Intrinsic
fromName name pos = case name of
  "print" -> Print
  "here"  -> Here pos
  "exit"  -> Exit