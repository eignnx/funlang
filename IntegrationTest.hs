import           Parser
import           TyCheck
import           Text.Parsec
import           Control.Monad.State

go src = do
  let ast = parseString src
  print $ evalState (infer ast) initState

-- FIXME: This throws error due to Block parsing.
-- def main[] -> Int do if false then 2 else 1 end end