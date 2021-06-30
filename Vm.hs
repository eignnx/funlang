module Vm
  ( execVmProgram
  , Stack
  , Memory
  , VmState
  )
where

import qualified Lir
import qualified Intr

import qualified Data.Foldable
import qualified Data.Map.Strict               as M
import           Control.Monad                  ( unless )
import           Control.Monad.Reader
import           Control.Monad.State

type Stack = [Lir.Value]
type Memory = [M.Map String Lir.Value]

data VmState = VmState
  { memory :: Memory
  , stack  :: Stack
  , pc     :: Lir.InstrAddr
  , instrs :: [Lir.Instr]
  }
  deriving Show

type VmProgram = StateT VmState IO

pop :: VmProgram Lir.Value
pop = do
  state <- get
  case stack state of
    (top : stack') -> do
      put $ state { stack = stack' }
      return top
    [] -> error $ "\n\tVM Stack Underflow!\n\tstate = " ++ show state ++ "\n"

popInt :: VmProgram Int
popInt = do
  val <- pop
  let Lir.VInt x = val
  return x

popBool :: VmProgram Bool
popBool = do
  val <- pop
  let Lir.VBool x = val
  return x

push :: Lir.Value -> VmProgram ()
push value = do
  state <- get
  put $ state { stack = value : stack state }

load :: String -> VmProgram ()
load var = do
  state <- get
  let (frame : _) = memory state
  case M.lookup var frame of
    Just val -> push val
    Nothing  -> error ("Unbound variable \"" ++ var ++ "\"")

store :: String -> VmProgram ()
store var = do
  val   <- pop
  state <- get
  let (frame : parentFrames) = memory state
  let frame'                 = M.insert var val frame
  put $ state { memory = frame' : parentFrames }

setPc :: Lir.InstrAddr -> VmProgram ()
setPc pc' = do
  modify $ \state -> state { pc = pc' }

incrPc :: VmProgram ()
incrPc = do
  oldPc <- gets pc
  setPc (oldPc + 1)

stepIntBinOp :: (Int -> Int -> Int) -> VmProgram ()
stepIntBinOp op = do
  x <- popInt
  y <- popInt
  push $ Lir.VInt $ x `op` y

stepIntBoolBinOp :: (Int -> Int -> Bool) -> VmProgram ()
stepIntBoolBinOp op = do
  x <- popInt
  y <- popInt
  push $ Lir.VBool $ x `op` y

stepBoolBinOp :: (Bool -> Bool -> Bool) -> VmProgram ()
stepBoolBinOp op = do
  x <- popBool
  y <- popBool
  push $ Lir.VBool $ x `op` y

stepVm :: Lir.Instr -> VmProgram ()
stepVm instr = do
  step
  incrPc
 where
  step = case instr of
    Lir.Load var -> do
      load var
    Lir.Store var -> do
      store var
    Lir.Const val -> do
      push val
    Lir.Dup -> do
      tmp <- pop
      push tmp
      push tmp
    -- [a, b, ...] -> [b, a, b, ...]
    Lir.Over -> do
      a <- pop
      b <- pop
      push b
      push a
      push b
    -- [a, b, c, ...] -> [c, a, b, ...]
    Lir.Rot -> do
      a <- pop
      b <- pop
      c <- pop
      push c
      push a
      push b
    Lir.Add -> stepIntBinOp (+)
    Lir.Sub -> stepIntBinOp (-)
    Lir.Mul -> stepIntBinOp (*)
    Lir.Div -> stepIntBinOp div
    Lir.Neg -> do
      i <- popInt
      push (Lir.VInt (-i))
    Lir.Eq  -> stepIntBoolBinOp (==)
    Lir.Gt  -> stepIntBoolBinOp (>)
    Lir.Lt  -> stepIntBoolBinOp (<)
    Lir.And -> stepBoolBinOp (&&)
    Lir.Or  -> stepBoolBinOp (||)
    Lir.Not -> do
      b <- popBool
      push (Lir.VBool (not b))
    Lir.Jmp        idx -> setPc (idx - 1)
    Lir.JmpIfFalse idx -> do
      b <- popBool
      unless b $
                 -- we always incr pc, so goto idx - 1
                 setPc (idx - 1)
    Lir.Nop -> do
      return ()
    Lir.Intrinsic intr -> runIntrinsic intr

runIntrinsic :: Intr.Intrinsic -> VmProgram ()
runIntrinsic op = case op of
  Intr.Print -> do
    x <- pop
    lift $ Lir.displayValue x
  Intr.Here pos -> do
    lift $ putStrLn ("intr.here[] at " ++ show pos)

testProgram =
  [ Lir.Const (Lir.VInt 0)
  , Lir.Store "x"
  , Lir.Load "x" -- <<<
  , Lir.Intrinsic Intr.Print
  , Lir.Load "x"
  , Lir.Const (Lir.VInt 1)
  , Lir.Add
  , Lir.Store "x"
  , Lir.Load "x"
  , Lir.Const (Lir.VInt 5)
  , Lir.Gt
  , Lir.JmpIfFalse 13
  , Lir.Jmp 2
  , Lir.Const (Lir.VBool True) -- <<<
  , Lir.Intrinsic Intr.Print
  ]

nth :: (Ord i, Num i) => [a] -> i -> Maybe a
nth _ n | n < 0 = Nothing
nth []       n  = Nothing
nth (x : xs) 0  = Just x
nth (_ : xs) n  = nth xs (n - 1)

debugStepProgram :: VmProgram ()
debugStepProgram = do
  pc1     <- gets pc
  instrs1 <- gets instrs
  st      <- get
  lift $ putChar '\t'
  lift $ print st
  lift $ putStr "\texecuting instr: "
  lift $ print (nth instrs1 pc1)
  Data.Foldable.mapM_ stepVm (nth instrs1 pc1)

runProgram :: VmProgram ()
runProgram = do
  pc1     <- gets pc
  instrs1 <- gets instrs
  case nth instrs1 pc1 of
    Just instr -> do
      stepVm instr
      runProgram
    Nothing -> return ()

initState :: [Lir.Instr] -> VmState
initState instrs =
  VmState { memory = [M.empty], stack = [], pc = 0, instrs = instrs }

execVmProgram :: [Lir.Instr] -> IO VmState
execVmProgram program = execStateT runProgram (initState program)