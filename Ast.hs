{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ast
  ( Ast
  , TypedAst(..)
  , ItemG(..)
  , Item(..)
  , TypedItem(..)
  , itemName
  , BinOp(..)
  , ArithOp(..)
  , BoolOp(..)
  , RelOp(..)
  , OtherOp(..)
  , Lit(..)
  , UnaryOp(..)
  , ExprF(..)
  , Fix(..)
  , Seq(..)
  , Expr
  , isEndTerminated
  , pattern Var
  , pattern Literal
  , pattern Unary
  , pattern Binary
  , pattern Block
  , pattern Call
  , pattern Intrinsic
  , pattern Let
  , pattern Assign
  , pattern Ret
  , pattern If
  , pattern While
  , pattern Loop
  , pattern Nop
  , pattern Ann
  , Typed(..)
  , RecTyped(..)
  , TypedExpr
  )
where

import qualified Ty
import qualified Text.ParserCombinators.Parsec.Pos as Parsec
import           Data.List                         ( isPrefixOf, intercalate )

type Ast = [Item]
type TypedAst = Typed [TypedItem]

instance Show TypedAst where
  show (items `HasTy` ty) = "Mod(" ++ show ty ++ "):" ++ indent ("\n" ++ unlines (map show items))

data ItemG e = Def String [(String, Ty.Ty)] (e, Maybe Ty.Ty)
type Item = ItemG Expr
type TypedItem = Typed (ItemG TypedExpr)

instance (Show e) => Show (ItemG e) where
  show (Def name paramsAndTys (body, Just retTy)) =
    "\ndef " ++ name ++ "[" ++ showParams paramsAndTys ++ "] -> " ++ show retTy ++ " " ++ show body
  show (Def name paramsAndTys (body, Nothing)) =
    "\ndef " ++ name ++ "[" ++ showParams paramsAndTys ++ "] = " ++ indent (show body)

showParams :: [(String, Ty.Ty)] -> String
showParams params = intercalate ", " $ map pairFmt params
  where pairFmt (param, ty) = param ++ ": " ++ show ty

instance Show TypedItem where
  show (item `HasTy` ty) = "Item(" ++ show ty ++ "):" ++ indent (show item)

itemName :: ItemG e -> String
itemName (Def name _ _) = name

data BinOp
  = ArithOp ArithOp
  | BoolOp BoolOp
  | RelOp RelOp
  | OtherOp OtherOp
  deriving (Show)

data ArithOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

data BoolOp
  = And
  | Or
  | Xor
  deriving (Show)

data RelOp
  = Gt
  | Lt
  | Eq
  | Neq
  deriving (Show, Eq)

data OtherOp
  = Concat
  deriving (Show)

data Lit
  = Bool Bool
  | Int Integer
  | String String
  | Unit
  deriving (Show)

data UnaryOp
  = Not
  | Neg
  deriving (Show)

-- | Represends a `do ... end` block.
--   The block:
--   ```
--   do
--     f[];
--     g[]
--   end
--   ```
--  translates to: `Semi g[] (Result g[])`.
--  The block:
--   ```
--   do
--     f[];
--     g[];
--   end
--   ```
--  translates to: `Semi g[] (Semi g[] Empty)`.
--  The block `do end` translates to: `Empty`.
data Seq e         -- <sequence> -->
  = Empty          --            | lookahead{END}
  | Result e       --            | <expr>
  | Semi e (Seq e) --            | <expr> SEMICOLON <sequence>

instance (IsEndTerminated e, Show e) => Show (Seq e)  where
  show Empty = ""
  show (Result e) = show e
  show (Semi e seq)
    | isEndTerminated e = show e ++ "\n" ++ show seq
    | otherwise = show e ++ ";\n" ++ show seq

-- This type used to be called `Expr` (see [this commit](1)), but was rewritten
-- to use an F-Algebra (I think that's right?), and backwards-compatible
-- patterns were added according to [this article](2).
-- [1]: https://github.com/eignnx/funlang/blob/9523a850776b9137f9a1c8295e4ee1100edeb98c/Ast.hs#L65-L73
-- [2]: https://mpickering.github.io/posts/2014-11-27-pain-free.html
data ExprF r
  = VarF String
  | LiteralF Lit
  | UnaryF UnaryOp r
  | BinaryF BinOp r r
  | BlockF (Seq r)
  | CallF r [r]
  | IntrinsicF Parsec.SourcePos String [r]
  | LetF String r
  | AssignF String r
  | RetF r
  | IfF r r r
  | WhileF r r
  | LoopF r
  | NopF
  | AnnF r Ty.Ty

newtype Fix f = Fix (f (Fix f))
unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

type Expr = Fix ExprF

class IsEndTerminated a where
  isEndTerminated :: a -> Bool

instance IsEndTerminated Expr where
  isEndTerminated e = isEndTerminated $ unfix e

instance IsEndTerminated TypedExpr where
  isEndTerminated e = isEndTerminated $ unRecTyped e

instance IsEndTerminated (ExprF f) where
  isEndTerminated (IfF _ _ _) = True
  isEndTerminated (WhileF _ _) = True
  isEndTerminated (LoopF _) = True
  isEndTerminated (BlockF _) = True
  isEndTerminated _ = False


pattern Var :: String -> Expr
pattern Var name = Fix (VarF name)
pattern Literal :: Lit -> Expr
pattern Literal x = Fix (LiteralF x)
pattern Unary :: UnaryOp -> Expr -> Expr
pattern Unary op x = Fix (UnaryF op x)
pattern Binary :: BinOp -> Expr -> Expr -> Expr
pattern Binary op x y = Fix (BinaryF op x y)
pattern Block :: Seq Expr -> Expr
pattern Block seq = Fix (BlockF seq)
pattern Call :: Expr -> [Expr] -> Expr
pattern Call f args = Fix (CallF f args)
pattern Intrinsic :: Parsec.SourcePos -> String -> [Expr] -> Expr
pattern Intrinsic pos name args = Fix (IntrinsicF pos name args)
pattern Let :: String -> Expr -> Expr
pattern Let name expr = Fix (LetF name expr)
pattern Assign :: String -> Expr -> Expr
pattern Assign name expr = Fix (AssignF name expr)
pattern Ret :: Expr -> Expr
pattern Ret x = Fix (RetF x)
pattern If :: Expr -> Expr -> Expr -> Expr
pattern If cond yes no = Fix (IfF cond yes no)
pattern While :: Expr -> Expr -> Expr
pattern While cond body = Fix (WhileF cond body)
pattern Loop :: Expr -> Expr
pattern Loop body = Fix (LoopF body)
pattern Nop :: Expr
pattern Nop = Fix NopF
pattern Ann :: Expr -> Ty.Ty -> Expr
pattern Ann expr ty = Fix (AnnF expr ty)

data Typed a = a `HasTy` Ty.Ty
data RecTyped f =  (f (RecTyped f)) `RecHasTy` Ty.Ty

unRecTyped :: RecTyped f -> f (RecTyped f)
unRecTyped (f `RecHasTy` _) = f

type TypedExpr = RecTyped ExprF

indent :: String -> String
indent txt = replace "\n" "\n  " txt
  where 
    -- From: https://programming-idioms.org/idiom/63/replace-fragment-of-a-string/976/haskell
    replace _ _ [] = []
    replace from to input = if isPrefixOf from input
      then to ++ replace from to (drop (length from) input)
      else head input : replace from to (tail input)

instance (Show (f ExprF), IsEndTerminated (f ExprF)) => Show (ExprF (f ExprF)) where
  show (VarF name) = name
  show (LiteralF (Int x)) = show x
  show (LiteralF (Bool x)) = if x then "true" else "false"
  show (LiteralF (String x)) = show x
  show (UnaryF Not x) = "not " ++ show x
  show (UnaryF Neg x) = "-(" ++ show x ++ ")"
  show (BinaryF op x y) = case op of
    ArithOp Add    -> show x ++ " + " ++ show y
    ArithOp Sub    -> show x ++ " - " ++ show y
    ArithOp Mul    -> show x ++ " * " ++ show y
    ArithOp Div    -> show x ++ " / " ++ show y
    BoolOp And     -> show x ++ " and " ++ show y
    BoolOp Or      -> show x ++ " or " ++ show y
    BoolOp Xor     -> show x ++ " xor " ++ show y
    RelOp Gt       -> show x ++ " > " ++ show y
    RelOp Lt       -> show x ++ " < " ++ show y
    RelOp Eq       -> show x ++ " == " ++ show y
    RelOp Neq      -> show x ++ " != " ++ show y
    OtherOp Concat -> show x ++ " ++ " ++ show y
  show (BlockF Empty) = "do end"
  show (BlockF seq) = "do" ++ indent ("\n" ++ show seq) ++ "\nend"
  show (CallF f args) = show f ++ show args
  show (IntrinsicF pos name args) = "intr." ++ name ++ show args
  show (LetF name e) = "let " ++ name ++ " = " ++ show e
  show (AssignF name e) = name ++ " = " ++ show e
  show (RetF e) = "ret " ++ show e
  show (IfF cond yes no) = "if " ++ show cond ++ " then" ++ indent ("\n" ++ show yes) ++ "\nelse" ++ indent ("\n" ++ show no) ++ "\nend"
  show (WhileF cond body) = "while " ++ show cond ++ " " ++ show body
  show (LoopF body) = "loop " ++ show body
  show NopF = "nop"
  show (AnnF e t) = show e ++ ": " ++ show t

instance Show TypedExpr where
  show (e `RecHasTy` t) = "(" ++ show e ++ " : " ++ show t ++ ")"

instance Show Expr where
  show (Fix e) = show e
