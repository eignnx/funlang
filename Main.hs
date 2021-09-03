{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Ast
import qualified Parser
import qualified Hir
import qualified TyCheck
import qualified Res
import qualified TastToHir
import qualified Lir
import qualified ToLir
import qualified Vm

import           System.Exit                    ( exitWith, ExitCode(..) )
import           System.Environment             ( getArgs )
import           System.Process                 ( createProcess, proc, CreateProcess(..), StdStream(CreatePipe, Inherit, UseHandle, NoStream), waitForProcess, createPipe )
import           System.FilePath                ( (</>) )
import           Data.List                      ( find, isPrefixOf, stripPrefix )
import           Control.Monad                  ( when, forM_ )
import           Text.Printf                    ( printf )
import System.IO (hPutStr, hClose, hPutStrLn, IOMode (ReadMode, WriteMode))
import Hir (Instr((:#)))
import GHC.IO.Handle.FD (openFile)
import System.Win32 (createFile)

data VmImpl = Hs | Rs
  deriving (Show, Eq)

data Opts
  = Opts { filename         :: String
         , traceCompilation :: Bool
         , traceVm          :: Bool
         , vmImpl           :: VmImpl
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
      let vmImpl = case () of
                        () | flags `hasSubFlag` ("-M", "-h", "--haskell-vm") -> Hs
                           | flags `hasSubFlag` ("-M", "-r", "--rust-vm") -> Rs
                           | otherwise -> Rs
      return $ Opts { filename = filename
                    , traceCompilation = traceCompilation
                    , traceVm = traceVm
                    , vmImpl = vmImpl
                    }

main :: IO ()
main = do
  opts <- getOpts
  ast  <- parseFile opts
  tast <- astToTypedAst opts ast
  hir  <- tastToHir opts tast
  lir  <- hirToLir opts hir
  execVmProgram opts lir

parseFile :: Opts -> IO Ast.Expr
parseFile opts = do
  ast <- Parser.parseFile $ filename opts
  when (traceCompilation opts) $ do
    putStrLn "\n===AST==="
    printAst ast
  return ast

astToTypedAst :: Opts -> Ast.Expr -> IO Ast.TypedExpr
astToTypedAst opts ast = do
  case TyCheck.astToTypedAst ast of
    Res.Ok tast -> do
      when (traceCompilation opts) $ do
        putStrLn "\n===TAST==="
        printTypedAst tast
      return tast
    Res.Err err -> do
      putStrLn $ "\n===COMPILATION ERROR===\n" ++ show err ++ "\n======================="
      exitWith (ExitFailure 1)


tastToHir :: Opts -> Ast.TypedExpr -> IO [Hir.Instr]
tastToHir opts tast = do
  let hir = TastToHir.astToHir tast
  when (traceCompilation opts) $ do
    putStrLn "\n===HIR==="
    printHir hir
  return hir

hirToLir :: Opts -> [Hir.Instr] -> IO [Lir.Instr]
hirToLir opts hir = do
  let lir = ToLir.hirToLir hir
  when (traceCompilation opts) $ do
    putStrLn "\n===LIR==="
    printLir lir
  return lir

execVmProgram :: Opts -> [Lir.Instr] -> IO ()
execVmProgram opts lir | vmImpl opts == Hs = do
  finalState <- if traceVm opts
    then Vm.debugExecVmProgram lir
    else Vm.execVmProgram lir
  return ()

execVmProgram opts lir | vmImpl opts == Rs = do
  let bcFileName = filename opts ++ ".bc"
  writeByteCodeToTmpFile lir bcFileName
  runRustSubprocess opts bcFileName

runRustSubprocess opts bcFileName = do
  let releaseArg = ["--release"]
  let traceArg = ["--trace-vm" | traceVm opts]
  let exe = "." </> "vm-rs" </> "target" </> "release" </> "vm-rs.exe"
  let cmd = proc exe $ bcFileName : traceArg
  (_, _, _, h) <- createProcess cmd
  waitForProcess h
  return ()

writeByteCodeToTmpFile lir bcFileName = do
  bcFile <- openFile bcFileName WriteMode
  forM_ (zip lir [0..]) $ \(instr, idx) ->
    hPutStrLn bcFile $ show idx ++ ": " ++ show instr
  hClose bcFile

showAssoc assoc = unlines $ ["{"] ++ pairs ++ ["}"]
  where
    pairs = map formatter assoc
    formatter (key, val) = "\t" ++ show key ++ " = " ++ show val

printAst :: Ast.Expr -> IO ()
printAst = print

printTypedAst :: Ast.TypedExpr -> IO ()
printTypedAst = print

printHir :: [Hir.Instr] -> IO ()
printHir hir = putStrLn $ unlines $ map f hir
  where
    f instr@(Hir.Label _ :# _) = show instr
    f instr@(Hir.Label _) = show instr
    f instr = "  " ++ show instr

printLir :: [Lir.Instr] -> IO ()
printLir lir = putStrLn $ unlines $ zipWith fmt [0..] lir
  where
    fmt :: Int -> Lir.Instr -> String
    fmt i instr = printf "%3d: %s" i (show instr)

compileAndRun :: Ast.Expr -> IO ()
compileAndRun ast = do
  let tast      = case TyCheck.astToTypedAst ast of
                       Res.Ok tast -> tast
                       Res.Err err -> error $ show err
  let hirInstrs = TastToHir.astToHir tast
  let lirInstrs = ToLir.hirToLir hirInstrs
  Vm.execVmProgram lirInstrs
  return ()

runProgram :: String -> IO ()
runProgram src = do
  ast <- Parser.parseString src
  compileAndRun ast
