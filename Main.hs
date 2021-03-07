module Main where

import qualified Compile                       as Ir
import           Control.Monad.State            ( evalState )
import           Control.Monad.Trans            ( lift )
import qualified Parser
import           System.Environment             ( getArgs )
import qualified Vm

compileAndRun :: Parser.Stmt -> IO ()
compileAndRun ast = do
    let irInstrs = Ir.irFromAst ast
    let vmInstrs = Vm.vmFromIr irInstrs
    Vm.execVmProgram vmInstrs
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

printAst src = print $ Parser.parseString src

printIr src = putStrLn $ unlines $ map show ir
  where
    ast = Parser.parseString src
    ir  = Ir.irFromAst ast

printVm src = putStrLn $ unlines $ map show vm
  where
    ast = Parser.parseString src
    ir  = Ir.irFromAst ast
    vm  = Vm.vmFromIr ir

runProgram src = compileAndRun $ Parser.parseString src

test =
    "n = 100; i = 1; j = 0; intr.print[999]; intr.here[]; while n > 0 do     intr.here[];     tmp = i + j;     j = i;     i = tmp;     n = n - 1; end; intr.print[i];"
