module Intr
  (Intrinsic(..)
  )
where

import qualified Text.ParserCombinators.Parsec.Pos as Parsec

data Intrinsic
  = Print
  | Here Parsec.SourcePos
  deriving Show