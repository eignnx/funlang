module Vm
  ( execVmProgram
  , debugExecVmProgram
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

import qualified Debug.Trace as Debug

type Stack = [Lir.Value]
type Memory = [M.Map String Lir.Value]

data VmState = VmState
  { memory  :: Memory
  , stack   :: Stack
  , pc      :: Lir.InstrAddr
  , instrs  :: [Lir.Instr]
  , running :: Bool
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

popString :: VmProgram String
popString = do
  val <- pop
  let Lir.VString x = val
  return x

popInstrAddr :: VmProgram Lir.InstrAddr
popInstrAddr = do
  val <- pop
  case val of
    Lir.VInstrAddr x -> return x
    other -> error $ "Interal Vm Error: Expected VInstrAddr, got: " ++ show other

push :: Lir.Value -> VmProgram ()
push value = do
  state <- get
  put $ state { stack = value : stack state }

load :: String -> VmProgram ()
load var = do
  state <- get
  push $ findInFrames $ memory state
    where
      findInFrames [] = error ("Unbound variable \"" ++ var ++ "\"")
      findInFrames (frame:frames) =
        case M.lookup var frame of
             Just val -> val
             Nothing  -> findInFrames frames

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

pushNewFrame :: VmProgram ()
pushNewFrame = do
  oldMem <- gets memory
  let newMem = M.empty : oldMem
  modify $ \state -> state { memory = newMem }

popMemFrame :: VmProgram ()
popMemFrame = do
  oldMem <- gets memory
  let (_ : newMem) = oldMem
  modify $ \state -> state { memory = newMem }

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
    -- [a, b, ...] -> [b, a, ...]
    Lir.Swap -> do
      a <- pop
      b <- pop
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
    Lir.Concat -> do
      a <- popString
      b <- popString
      push $ Lir.VString $ a ++ b
    Lir.Jmp        idx -> setPc (idx - 1)
    Lir.JmpIfFalse idx -> do
      b <- popBool
      unless b $
                 -- we always incr pc, so goto idx - 1
                 setPc (idx - 1)
    Lir.Nop -> return ()
    Lir.Intrinsic intr -> runIntrinsic intr
    -- Assume stack is set up properly beforehand.
    -- Stack should look like this:
    --  | <the function's InstrAddr>   <-- TOS
    --  | <arg n>
    --  | <arg n-1>
    --  | ...
    --  | <arg 2>
    --  | <arg 1>
    -- After running Lir.Call, stack should like like this:
    --  | <return address>
    --  | <arg n>
    --  | <arg n-1>
    --  | ...
    --  | <arg 2>
    --  | <arg 1>
    Lir.Call argC -> do
      fnAddr <- popInstrAddr
      -- Store the return address above all the args.
      pc_ <- gets pc
      let retAddr = pc_ + 1 -- Return to the NEXT instr.
      push $ Lir.VInstrAddr retAddr
      -- Perform the jump.
      setPc (fnAddr - 1)
      pushNewFrame
      return ()
    Lir.Ret -> do
      retAddr <- popInstrAddr
      popMemFrame
      setPc (retAddr - 1)
      return ()

runIntrinsic :: Intr.Intrinsic -> VmProgram ()
runIntrinsic op = case op of
  Intr.Print -> do
    x <- pop
    lift $ Lir.displayValue x
  Intr.Here pos -> do
    lift $ putStrLn ("intr.here[] at " ++ show pos)
  Intr.Exit -> do
    modify $ \state -> state { running = False }

nth :: (Ord i, Num i) => [a] -> i -> Maybe a
nth _ n | n < 0 = Nothing
nth []       n  = Nothing
nth (x : xs) 0  = Just x
nth (_ : xs) n  = nth xs (n - 1)

debugStepProgram :: Lir.Instr -> VmProgram ()
debugStepProgram instr = do
  instrs_  <- gets instrs
  pc_      <- gets pc
  stack_   <- gets stack
  memory_  <- gets memory
  let Lir.InstrAddr pcNum = pc_
  lift $ putStrLn $ "\tInstr #" ++ show pcNum ++ ": " ++ show instr
  lift $ putStr "\tStack: "
  lift $ print stack_
  lift $ putStr "\tMemory: "
  lift $ print memory_
  stepVm instr

debugRunProgram :: VmProgram ()
debugRunProgram = do
  pc_     <- gets pc
  instrs_ <- gets instrs
  vmState <- get
  when (running vmState) $
    case nth instrs_ pc_ of
      Just instr -> do
        debugStepProgram instr
        lift $ putStr "\tPress ENTER to step forward...\n"
        input <- lift $ getLine
        if input == "q" || input == "Q" || input == "quit"
          then return ()
          else debugRunProgram
      Nothing -> error $ "VM: Illegal instruction address: " ++ show pc_

runProgram :: VmProgram ()
runProgram = do
  pc_     <- gets pc
  instrs_ <- gets instrs
  vmState <- get
  when (running vmState) $
    case nth instrs_ pc_ of
      Just instr -> do
        stepVm instr
        runProgram
      Nothing -> error $ "VM: Illegal instruction address: " ++ show pc_

initState :: [Lir.Instr] -> VmState
initState instrs =
  VmState { memory = [M.empty]
          , stack = []
          , pc = 0
          , instrs = instrs
          , running = True
          }

execVmProgram :: [Lir.Instr] -> IO VmState
execVmProgram program = execStateT runProgram (initState program)

debugExecVmProgram :: [Lir.Instr] -> IO VmState
debugExecVmProgram program = execStateT debugRunProgram (initState program)