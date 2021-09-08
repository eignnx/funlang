{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module LibFunlang
  ( Opts (..),
    getOpts,
    parseAndCompile,
    execVmProgram,
    runRustSubprocess,
    writeByteCodeToTmpFile,
  )
where

import qualified Ast
import Control.Monad (forM_, when)
import Data.List (find, isPrefixOf, stripPrefix)
import Hir (Instr ((:#)))
import qualified Hir
import qualified Lir
import qualified Parser
import qualified Res
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile)
import System.Process (createProcess, proc, waitForProcess)
import qualified TastToHir
import Text.Printf (printf)
import qualified ToLir
import qualified TyCheck

data Opts = Opts
  { filename :: String,
    traceCompilation :: Bool,
    traceVm :: Bool
  }
  deriving (Show)

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
      let traceCompilation =
            flags `hasSubFlag` ("-T", "c", "--trace-compilation")
              || flags `hasFlag` ("-T", "--trace-all")
      let traceVm =
            flags `hasSubFlag` ("-T", "v", "--trace-vm")
              || flags `hasFlag` ("-T", "--trace-all")
      return $
        Opts
          { filename = filename,
            traceCompilation = traceCompilation,
            traceVm = traceVm
          }

parseAndCompile opts = do
  ast <- parseFile opts
  tast <- astToTypedAst opts ast
  hir <- tastToHir opts tast
  hirToLir opts hir

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
  let hir = TastToHir.tastToHir tast
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
execVmProgram opts lir = do
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
  forM_ (zip lir [0 ..]) $ \(instr, idx) ->
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
printLir lir = putStrLn $ unlines $ zipWith fmt [0 ..] lir
  where
    fmt :: Int -> Lir.Instr -> String
    fmt i instr = printf "%3d: %s" i (show instr)
