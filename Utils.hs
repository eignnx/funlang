module Utils
  ( (+++)
  , code
  , codeIdent
  , indent
  )
where

import Data.List ( isPrefixOf )

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
