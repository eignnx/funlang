module Main where

import qualified Parser
import qualified ToHir
import qualified ToLir
import qualified Vm

import           System.Environment             ( getArgs )

-- compileAndRun :: Parser.Stmt -> IO ()
compileAndRun ast = do
  let hirInstrs = ToHir.astToHir ast
  let lirInstrs = ToLir.hirToLir hirInstrs
  Vm.execVmProgram lirInstrs
  return ()

getFilename :: IO String
getFilename = do
  args <- getArgs
  case args of
    [filename] -> return filename
    []         -> error "Please provide the path to a source file!"
    _ ->
      error
        (  "too many arguments provided! Expected 1, got "
        ++ show (length args)
        )

main :: IO ()
main = do
  filename <- getFilename
  src      <- readFile filename
  let ast = Parser.parseSrc filename src
  compileAndRun ast

printAst :: String -> IO ()
printAst src = print $ Parser.parseString src

showAssoc assoc = unlines $ ["{"] ++ pairs ++ ["}"]
  where
    pairs = map formatter assoc
    formatter (key, val) = "\t" ++ show key ++ " = " ++ show val

printHir :: String -> IO ()
printHir src = putStrLn $ unlines $ header ++ map show hir
  where
    ast    = Parser.parseString src
    header = ["Defs = " ++ showAssoc (ToHir.getDefs ast)]
    hir     = ToHir.astToHir ast

printLir :: String -> IO ()
printLir src = putStrLn $ unlines $ map show lir
  where
    ast = Parser.parseString src
    hir  = ToHir.astToHir ast
    lir  = ToLir.hirToLir hir

runProgram :: String -> IO ()
runProgram src = compileAndRun $ Parser.parseString src
