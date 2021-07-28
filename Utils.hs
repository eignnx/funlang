module Utils
  ( (+++)
  , code
  , codeIdent
  , indent
  , Span(..)
  , mkSpan
  )
where

import Data.List ( isPrefixOf )
import Text.Parsec.Pos ( SourcePos, sourceName, sourceLine, sourceColumn )

(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b

code :: Show a => a -> String
code a = let
  txt = show a
  in if '\n' `elem` txt
      then "\n```\n" ++ txt ++ "\n```\n"
      else "`" ++ txt ++ "`"

codeIdent :: String -> String
codeIdent a = "`" ++ a ++ "`"

indent :: String -> String
indent txt = replace "\n" "\n  " ("\n" ++ txt)
  where 
    -- From: https://programming-idioms.org/idiom/63/replace-fragment-of-a-string/976/haskell
    replace _ _ [] = []
    replace from to input = if isPrefixOf from input
      then to ++ replace from to (drop (length from) input)
      else head input : replace from to (tail input)

data Span = Span String (Int, Int) (Int, Int)
  deriving Eq

instance Show Span where
  show (Span file start end) = show file +++ "from" +++ pair start +++ "to" +++ pair end
    where pair (a, b) = show a ++ ":" ++ show b

mkSpan :: SourcePos -> SourcePos -> Span
mkSpan p1 p2 = let
  name = sourceName p1
  p1' = (sourceLine p1, sourceColumn p1)
  p2' = (sourceLine p2, sourceColumn p2)
  in Span name p1' p2'
