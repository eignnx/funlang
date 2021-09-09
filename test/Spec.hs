{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

import Ast
import Cata (At (..))
import Control.Monad (filterM, forM, forM_, liftM, liftM2, replicateM)
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as S
import GHC.Generics
import qualified GHC.Generics
import GHC.TopHandler (runIO)
import qualified Lir
import qualified Parser
import qualified Res
import System.FilePath ((</>))
import System.IO (hPrint, hPutStrLn, openTempFileWithDefaultPermissions)
import qualified TastToHir
import Test.QuickCheck (Arbitrary (shrink), Gen, NonEmptyList (NonEmpty), NonNegative (NonNegative), arbitrary, choose, chooseInt, classify, discard, elements, forAll, frequency, genericShrink, label, labelledExamples, oneof, quickCheckAll, resize, sized, vector, vectorOf, verboseCheck, verboseShrinking, withMaxSuccess, (===), (==>))
import Test.QuickCheck.Arbitrary (Arbitrary)
import Test.QuickCheck.Test (quickCheck)
import qualified TestingVm
import qualified ToLir
import Ty
import qualified TyCheck
import Utils (Span, mkSpan, (+++))

runVm ast =
  let tast = Res.unwrapRes $ TyCheck.astToTypedAst ast
      hir = TastToHir.tastToHir tast
      lir = ToLir.hirToLir hir
      output = TestingVm.runAndCollectOutputFromVmProgram lir
   in output

runExpr expr = runVm (exprBoilerplate expr)

exprBoilerplate :: Expr -> Expr
exprBoilerplate expr =
  at $
    ModF
      "Test"
      [ at $
          DefF
            "main"
            []
            (Just VoidTy)
            ( at $
                BlockF $ Result expr
            )
      ]

prop_xMinusXIs0 (IntExpr expr) = runExpr (dbgInt (expr `minus` expr)) === Right ["0"]

minus :: Expr -> Expr -> Expr
minus x y = at $ BinaryF (ArithOp Sub) x y

labelExeResult eith rest =
  case eith of
    Right _ -> label "Success" rest
    Left e -> label e rest

-- | See https://github.com/eignnx/funlang/issues/3
patHasNoInitialEmptyTuples :: Pat -> Bool
patHasNoInitialEmptyTuples = \case
  TuplePat [] -> False
  TuplePat ps -> all patHasNoInitialEmptyTuples ps
  VarPat _ -> True

-- | See https://github.com/eignnx/funlang/issues/3
exprHasNoInitialEmptyTuples :: Expr -> Bool
exprHasNoInitialEmptyTuples = \case
  TupleExpr [] -> False
  TupleExpr es -> all exprHasNoInitialEmptyTuples es
  _ -> True

-- | See https://github.com/eignnx/funlang/issues/3
noInitialEmptyTuples pat expr =
  patHasNoInitialEmptyTuples pat && exprHasNoInitialEmptyTuples expr

prop_simpleMatchHasNoRtErrs PatPair {pat, expr, bindings} =
  noInitialEmptyTuples pat expr ==> isRight (runExpr m)
  where
    m =
      match expr $
        [ (pat', [puts "matched!"]),
          (VarRefutPat "_", [puts "no match!", exit])
        ]
    pat' = patToRefutPat pat

prop_simpleLetElseHasNoRtErrs PatPair {pat, expr, bindings} =
  noInitialEmptyTuples pat expr ==> isRight (runExpr le)
  where
    le =
      block
        [ letElse pat' expr $
            [puts "no match!", exit],
          puts "matched!"
        ]
    pat' = patToRefutPat pat

prop_letElseMatchEquivalence PatPair {pat, expr, bindings} =
  labelExeResult rhs $
    noInitialEmptyTuples pat expr
      ==> rhs === lhs
  where
    rhs = runExpr m
    lhs = runExpr le
    m =
      match expr $
        [ (pat', [puts "matched!"]),
          (VarRefutPat "_", [puts "no match!", exit])
        ]
    le =
      block
        [ letElse pat' expr $
            [puts "no match!", exit],
          puts "matched!"
        ]
    pat' = patToRefutPat pat

block :: Foldable t => t Expr -> Expr
block es = at $ BlockF (mkSeq es)

mkSeq :: Foldable t => t e -> Seq e
mkSeq = foldr Semi Empty

exit = intrinsic "exit" []

letElse :: Foldable t => RefutPat -> At ExprF -> t (At ExprF) -> At ExprF
letElse pat expr alt = at $ LetElseF pat expr (mkSeq alt)

patToRefutPat :: Pat -> RefutPat
patToRefutPat = \case
  VarPat x -> VarRefutPat x
  TuplePat pats -> TupleRefutPat $ map patToRefutPat pats

match :: Foldable t => At ExprF -> [(RefutPat, t (At ExprF))] -> At ExprF
match expr arms = at $ MatchF expr arms'
  where
    arms' = map f arms
    f (pat, es) = (pat, mkSeq es)

puts msg = intrinsic "puts" [at $ LiteralF $ Text msg]

loc :: Span
loc = undefined

at :: f (At f) -> At f
at expr = expr :@: loc

intrinsic :: String -> [At ExprF] -> At ExprF
intrinsic name args = at $ IntrinsicF undefined name args

dbgInt expr = intrinsic "dbg_int" [expr]

newtype IntExpr = IntExpr Ast.Expr
  deriving (Show)

instance Arbitrary IntExpr where
  arbitrary = IntExpr <$> sized expr
    where
      expr 0 = leaf
      expr n =
        at
          <$> oneof
            [ UnaryF Neg <$> sub 2,
              BinaryF <$> arbitrary <*> sub 2 <*> sub 2
            ]
        where
          sub k = expr (n `div` k)
      leaf = at . LiteralF . Int <$> arbitrary

int x = at $ LiteralF $ Int x

pattern TupleExpr :: [Expr] -> Expr
pattern TupleExpr es <-
  LiteralF (Tuple es) :@: _
  where
    TupleExpr es = at $ LiteralF $ Tuple es

vars :: [String]
vars = take 1000 $ zipWith f (cycle ['a' .. 'z']) [0 ..]
  where
    f c n = c : show n

data PatPair = PatPair {pat :: Pat, expr :: Expr, bindings :: S.Set String}

instance Show PatPair where
  show PatPair {pat, expr, bindings} = show pat +++ "<~" +++ show expr

instance Arbitrary PatPair where
  arbitrary = sized gen
    where
      gen 0 = return $ PatPair {pat = TuplePat [], expr = TupleExpr [], bindings = S.empty}
      gen 1 = do
        name <- elements vars
        lit <- at . LiteralF <$> arbitrary
        return $ PatPair {pat = VarPat name, expr = lit, bindings = S.singleton name}
      gen n = tuplePatPair
        where
          tuplePatPair = do
            len <- chooseInt (0, n)
            patPairs <- replicateM len (gen (n `div` len))
            let init = ([], [], S.empty)
            let (ps, es, bindings) = foldr f init patPairs
            return $ PatPair {pat = TuplePat ps, expr = TupleExpr es, bindings}
            where
              f PatPair {pat = p1, expr = e1, bindings = b1} (ps, es, b0) =
                if S.null (S.intersection b0 b1) -- Ensure all pattern vars are unique.
                  then (p1 : ps, e1 : es, S.union b0 b1)
                  else discard

  shrink p@PatPair {pat = _, expr = TupleExpr [], bindings = bs} = []
  shrink p@PatPair {pat = VarPat _, expr = _, bindings = bs} = []
  shrink p@PatPair {pat = TuplePat [pat], expr = TupleExpr [expr], bindings = bs} =
    [PatPair {pat, expr, bindings = bs}]
  shrink p@PatPair {pat = TuplePat pats, expr = TupleExpr es, bindings = bs} = shrunkToVarPat : shrunkToSubPat
    where
      shrunkToVarPat = PatPair {pat = VarPat v, expr = TupleExpr es, bindings = S.insert v bs}
      shrunkToSubPat = [PatPair {pat = TuplePat pats', expr = TupleExpr es', bindings = bs} | (pats', es') <- subs]
      subs = map unzip $ init $ powerset $ zip pats es
      v = head $ filter (`notElem` bs) vars
  shrink _ = undefined

powerset = filterM (const [False, True])

newtype TupleGen = TupleGen Ast.Expr

tupleGen :: [Ty] -> Gen TupleGen
tupleGen tys = do
  vals <- forM tys $ \ty ->
    genValueOfType ty
  return $ TupleGen $ at $ LiteralF $ Tuple vals

patGen :: Ty -> Gen Pat
patGen ty = sized gen
  where
    gen 0 = leaf
    gen n = case ty of
      ValTy s -> leaf
      AliasTy s -> leaf
      -- VrntTy map -> _
      TupleTy tys -> do
        elems <- choose (0, n)
        let sub = gen (n `div` elems)
        TuplePat <$> replicateM elems sub
      _ -> undefined
    leaf = VarPat <$> elements vars

genValueOfType :: Ty -> Gen Ast.Expr
genValueOfType = \case
  -- NeverTy -> return $ at $ intrinsic "exit" [] -- could also do `loop`s
  VoidTy -> oneof $ map (pure . at) [NopF, LiteralF Unit]
  BoolTy -> frequency [(1, at . LiteralF . Bool <$> arbitrary)] -- could add binary relops and boolops here
  IntTy -> (\(IntExpr e) -> e) <$> arbitrary
  TextTy -> return $ at $ LiteralF $ Text "this is text"
  -- ValTy s -> _
  -- AliasTy s -> _
  VrntTy map -> undefined
  TupleTy tys -> (\(TupleGen e) -> e) <$> tupleGen tys
  -- FnTy tys ty -> _
  -- ModTy map -> _
  -- RecTy s ty -> _
  -- TyVar s -> _
  _ -> undefined

instance Arbitrary TupleGen where
  arbitrary = TupleGen . at . LiteralF . Tuple <$> sized tuple
    where
      tuple 0 = undefined
      tuple n = undefined

instance Arbitrary Ty where
  arbitrary = sized ty
    where
      leaf = oneof $ map pure [NeverTy, VoidTy, BoolTy, IntTy, TextTy]
      ty 0 = leaf
      ty n = oneof [leaf, tupleTy]
        where
          sub :: Int -> Gen Ty
          sub k = ty (n `div` k)
          tupleTy = do
            len <- chooseInt (0, n)
            tys <- replicateM len (sub len)
            return $ TupleTy tys

instance Arbitrary Pat where
  arbitrary = oneof $ map (pure . VarPat) ["a", "b", "c"]

instance Arbitrary (Lit a) where
  arbitrary = sized $ \case
    n ->
      oneof
        [ Int <$> arbitrary,
          -- return Unit,
          Bool <$> arbitrary,
          pure (Text "blah")
          -- Tuple [e]
          -- Vrnt String [e]
        ]

instance Arbitrary BinOp where
  arbitrary = oneof [ArithOp <$> arbitrary]

instance Arbitrary ArithOp where
  arbitrary = oneof $ pure <$> [Add, Sub, Mul]

------------------------------------------------------------------------------------------

return [] -- Template Haskell: Stop here!

main = $quickCheckAll
