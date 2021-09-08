module Main where

import LibFunlang (execVmProgram, getOpts, parseAndCompile)

main :: IO ()
main = do
  opts <- getOpts
  lir <- parseAndCompile opts
  execVmProgram opts lir