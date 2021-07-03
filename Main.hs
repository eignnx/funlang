{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Ast
import qualified Parser
import qualified Hir
import qualified ToHir
import qualified Lir
import qualified ToLir
import qualified Vm

import           System.Environment             ( getArgs )
import           Data.List                      ( find, isPrefixOf, stripPrefix )
import           Control.Monad                  ( when )
import           Text.Printf                    ( printf )

data Opts
  = Opts { filename         :: String
         , traceCompilation :: Bool
         , traceVm          :: Bool
         }
  deriving Show

hasFlag :: [String] -> (String, String) -> Bool
flags `hasFlag` (short, long) = short `elem` flags || long `elem` flags

hasSubFlag :: [String] -> (String, String, String) -> Bool
flags `hasSubFlag` (_, _, long) | long `elem` flags = True
flags `hasSubFlag` (super, sub, _) =
  case find (super `isPrefixOf`) flags of
    Nothing -> False
    Just flag -> sub `elem` stripPrefix super flag

getOpts :: IO Opts
getOpts = do
  args <- getArgs
  case args of
    [] -> error "Please provide the path to a source file!"
    (filename : flags) -> do
      let traceCompilation =  flags `hasSubFlag` ("-T", "c", "--trace-compilation")
                           || flags `hasFlag` ("-T", "--trace-all")
      let traceVm =  flags `hasSubFlag` ("-T", "v", "--trace-vm")
                  || flags `hasFlag` ("-T", "--trace-all")
      return $ Opts { filename = filename
                    , traceCompilation = traceCompilation
                    , traceVm = traceVm
                    } 

getDebugMode :: IO Bool
getDebugMode = do
  args <- getArgs
  case args of
    [filename, "-t"] -> return True
    [filename, "--trace-compilation"] -> return True

main :: IO ()
main = do
  opts <- getOpts
  src  <- readFile (filename opts)
  ast  <- parseSrc opts src
  hir  <- astToHir opts ast
  lir  <- hirToLir opts hir
  execVmProgram opts lir

parseSrc :: Opts -> String -> IO Ast.Ast
parseSrc opts src = do
  let ast = Parser.parseSrc (filename opts) src
  when (traceCompilation opts) $ do
    putStrLn "===AST==="
    printAst ast
  return ast

astToHir :: Opts -> Ast.Ast -> IO [Hir.Instr]
astToHir opts ast = do
  let hir = ToHir.astToHir ast
  when (traceCompilation opts) $ do
    putStrLn "===HIR==="
    printHir hir
  return hir

hirToLir :: Opts -> [Hir.Instr] -> IO [Lir.Instr]
hirToLir opts hir = do
  let lir = ToLir.hirToLir hir
  when (traceCompilation opts) $ do
    putStrLn "===LIR==="
    printLir lir
  return lir

execVmProgram :: Opts -> [Lir.Instr] -> IO ()
execVmProgram opts lir = do
  finalState <- if traceVm opts
    then Vm.debugExecVmProgram lir
    else Vm.execVmProgram lir
  return ()

showAssoc assoc = unlines $ ["{"] ++ pairs ++ ["}"]
  where
    pairs = map formatter assoc
    formatter (key, val) = "\t" ++ show key ++ " = " ++ show val

printAst :: Ast.Ast -> IO ()
printAst ast = putStrLn $ showAssoc (ToHir.getDefs ast) 

printHir :: [Hir.Instr] -> IO ()
printHir hir = putStrLn $ unlines $ map show hir

printLir :: [Lir.Instr] -> IO ()
printLir lir = putStrLn $ unlines $ zipWith fmt [0..] lir
  where
    fmt :: Int -> Lir.Instr -> String
    fmt i instr = printf "%3d: %s" i (show instr)

compileAndRun :: Ast.Ast -> IO ()
compileAndRun ast = do
  let hirInstrs = ToHir.astToHir ast
  let lirInstrs = ToLir.hirToLir hirInstrs
  Vm.execVmProgram lirInstrs
  return ()

runProgram :: String -> IO ()
runProgram src = compileAndRun $ Parser.parseString src
