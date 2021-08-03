{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Ast
  ( ExprF(..)
  , itemName
  , isModLevelItem
  , modLevelItemTy
  , Seq(..)
  , BinOp(..)
  , ArithOp(..)
  , BoolOp(..)
  , RelOp(..)
  , OtherOp(..)
  , Lit(..)
  , UnaryOp(..)
  , Expr
  , isEndTerminated
  , Typed(..)
  , TypedExpr
  )
where

import qualified Ty
import           Cata                              ( At(..), RecTyped(..), Unwrap(..), cata )
import           Utils                             ( (+++), code, indent )

import qualified Text.ParserCombinators.Parsec.Pos as Parsec
import           Data.List                         ( intercalate, isSuffixOf )
import qualified Data.Map                          as M

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
  | LetConstF String r
  | RetF r
  | IfF r r r
  | WhileF r r
  | LoopF r
  | NopF
  | AnnF r Ty.Ty
  | DefF String [(String, Ty.Ty)] (Maybe Ty.Ty) r
  | ModF String [r]
  deriving Functor

type Expr = At ExprF

itemName :: ExprF r -> Maybe String
itemName = \case
  DefF name _ _ _ -> Just name
  ModF name _     -> Just name
  LetConstF name _  -> Just name
  _               -> Nothing

isModLevelItem :: ExprF r -> Bool
isModLevelItem = \case
  DefF _ _ _ _ -> True
  ModF _ _     -> True
  LetConstF _ _  -> True
  _            -> False

modLevelItemTy :: TypedExpr -> Ty.Ty
modLevelItemTy = \case

  DefF _ params Nothing (_ :<: bodyTy) :<: _ ->
    Ty.FnTy (map snd params) bodyTy

  DefF _ params (Just retTy) _ :<: _ ->
    Ty.FnTy (map snd params) retTy

  ModF _ items :<: _ ->
    (Ty.ModTy $ M.fromList $ pairs)
      where getItemName (item :<: _) = maybe (error "") id $ itemName item
            pairs = zip (map getItemName items) (map modLevelItemTy items)

  LetConstF _ (exprF :<: ty) :<: _ -> ty -- Just return the type of `e` in `static x = e`.

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
  | Result e       --            | <expr> lookahead{END}
  | Semi e (Seq e) --            | <expr> SEMICOLON <sequence>
  deriving Functor

instance (IsEndTerminated e, Show e) => Show (Seq e)  where
  show = \case
    Empty -> ""
    Result e -> show e
    Semi e Empty
      | isEndTerminated e -> show e
      | otherwise -> show e ++ ";"
    Semi e seq
      | isEndTerminated e -> show e ++ "\n" ++ show seq
      | otherwise -> show e ++ ";\n" ++ show seq

class IsEndTerminated a where
  isEndTerminated :: a -> Bool

instance IsEndTerminated Expr where
  isEndTerminated e = isEndTerminated $ unwrap e

instance IsEndTerminated TypedExpr where
  isEndTerminated e = isEndTerminated $ unwrap e

instance IsEndTerminated (ExprF f) where
  isEndTerminated (IfF _ _ _) = True
  isEndTerminated (WhileF _ _) = True
  isEndTerminated (LoopF _) = True
  isEndTerminated (BlockF _) = True
  isEndTerminated _ = False

data Typed a = a `HasTy` Ty.Ty

type TypedExpr = RecTyped ExprF

showParams :: [(String, Ty.Ty)] -> String
showParams params = intercalate ", " $ map pairFmt params
  where pairFmt (param, ty) = param ++ ": " ++ show ty

instance (Show (f ExprF), IsEndTerminated (f ExprF)) => Show (ExprF (f ExprF)) where
  show (VarF name) = name
  show (LiteralF (Int x)) = show x
  show (LiteralF (Bool x)) = if x then "true" else "false"
  show (LiteralF (String x)) = show x
  show (UnaryF Not x) = "not " ++ show x
  show (UnaryF Neg x) = "-(" ++ show x ++ ")"
  show (BinaryF op x y) = case op of
    ArithOp Add    -> show x +++ "+" +++ show y
    ArithOp Sub    -> show x +++ "-" +++ show y
    ArithOp Mul    -> show x +++ "*" +++ show y
    ArithOp Div    -> show x +++ "/" +++ show y
    BoolOp And     -> show x +++ "and" +++ show y
    BoolOp Or      -> show x +++ "or" +++ show y
    BoolOp Xor     -> show x +++ "xor" +++ show y
    RelOp Gt       -> show x +++ ">" +++ show y
    RelOp Lt       -> show x +++ "<" +++ show y
    OtherOp Concat -> show x +++ "++" +++ show y
  show (BlockF Empty) = "do end"
  show (BlockF seq) = "do" ++ indent (show seq) ++ "\nend"
  show (CallF f args) = show f ++ show args
  show (IntrinsicF pos name args) = "intr." ++ name ++ show args
  show (LetF name e) = "let" +++ name +++ "=" +++ show e
  show (AssignF name e) = name +++ "=" +++ show e
  show (LetConstF name e) = "let const" +++ name +++ "=" +++ show e
  show (RetF e) = "ret" +++ show e
  show (IfF cond yes no) = "if" +++ show cond +++ "then" ++ indent (show yes) ++ "\nelse" ++ indent (show no) ++ "\nend"
  show (WhileF cond body) = "while" +++ show cond +++ show body
  show (LoopF body) = "loop" +++ show body
  show NopF = "nop"
  show (AnnF e t) = show e ++ ":" +++ show t
  show (DefF name paramsAndTys (Just retTy) body) =
    "def" +++ name ++ "[" ++ showParams paramsAndTys ++ "] ->" +++ show retTy +++ show body
  show (DefF name paramsAndTys Nothing body) =
    "def" +++ name ++ "[" ++ showParams paramsAndTys ++ "] =" +++ indent (show body)
  show (ModF name items) =
    "mod" +++ name ++ indent (intercalate "\n\n" (map show items)) ++ "\nend"

instance Show TypedExpr where
  show (e :<: t) = "(" ++ show e +++ ":" +++ show t ++ ")"

instance Show Expr where
  show (e :@: _) = show e
