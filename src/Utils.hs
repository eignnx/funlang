module Utils
  ( (+++),
    code,
    codeIdent,
    indent,
    brackets,
    braces,
    commaSep,
    list,
    optList,
    Span (..),
    mkSpan,
  )
where

import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Text.Parsec.Pos (SourcePos, sourceColumn, sourceLine, sourceName)

(+++) :: String -> String -> String
a +++ b
  | "\n" `isSuffixOf` a = a ++ b
  | b == "" = a
  | otherwise = a ++ " " ++ b

code :: Show a => a -> String
code a =
  let txt = show a
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
    replace from to input =
      if from `isPrefixOf` input
        then to ++ replace from to (drop (length from) input)
        else head input : replace from to (tail input)

brackets :: String -> String
brackets a = "[" ++ a ++ "]"

braces :: String -> String
braces a = "{" +++ a +++ "}"

commaSep :: [String] -> String
commaSep = intercalate ", "

list :: Show a => [a] -> String
list as = brackets $ commaSep $ map show as

optList :: Show a => [a] -> String
optList as
  | null as = ""
  | otherwise = list as

data Span = Span String (Int, Int) (Int, Int)
  deriving (Eq)

instance Show Span where
  show (Span file start end) = file +++ "from" +++ pair start +++ "to" +++ pair end
    where
      pair (a, b) = show a ++ ":" ++ show b

mkSpan :: SourcePos -> SourcePos -> Span
mkSpan p1 p2 =
  let name = sourceName p1
      p1' = (sourceLine p1, sourceColumn p1)
      p2' = (sourceLine p2, sourceColumn p2)
   in Span name p1' p2'
