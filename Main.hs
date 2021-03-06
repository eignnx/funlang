module Main where

import qualified Compile
import           Control.Monad.State            ( evalState )
import qualified Parser
import           System.Environment             ( getArgs )
import qualified Vm

compileAndRun :: String -> IO ()
compileAndRun src = do
    let ast      = Parser.parseString src
    let irInstrs = Compile.irFromAst ast
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
main = getFilename >>= readFile >>= compileAndRun
