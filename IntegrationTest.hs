import           Parser
import           TyCheck
import           TypedAstToHir
import           Text.Parsec
import           Control.Monad.State

go src = do
  let ast = parseString src
  print ast
  typedAst <- unwrapRes $ evalState (infer ast) initState
  print typedAst
  let hir = astToHir typedAst
  putStrLn $ unlines $ map show hir

unwrapRes :: TyCheck.Res a -> IO a
unwrapRes (TyCheck.Ok x) = return x
unwrapRes (TyCheck.Err e) = error $ show e

-- FIXME: This throws error due to Block parsing.
-- def main[] -> Int do if false then 2 else 1 end end