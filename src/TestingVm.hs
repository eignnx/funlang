module TestingVm
  ( execVmProgram,
    runAndCollectOutputFromVmProgram,
    Stack,
    Memory,
    VmState,
  )
where

import Control.Monad.Reader (MonadTrans (lift), unless, when)
import Control.Monad.State
  ( MonadState (get, put),
    StateT,
    execStateT,
    gets,
    modify,
  )
import Control.Monad.Writer (MonadWriter (tell), Writer, WriterT (runWriterT), execWriter, runWriter)
import qualified Data.Foldable
import qualified Data.Map.Strict as M
import qualified Debug.Trace as Debug
import qualified Intr
import qualified Lir
import Utils (code, codeIdent, (+++))

type Stack = [Lir.Value]

type Memory = [M.Map String Lir.Value]

data VmState = VmState
  { memory :: Memory,
    heap :: M.Map Int Lir.Value,
    stack :: Stack,
    retAddrs :: [Lir.InstrAddr],
    pc :: Lir.InstrAddr,
    instrs :: M.Map Int Lir.Instr,
    running :: Bool
  }
  deriving (Show)

type VmProgram = StateT VmState (WriterT String (Either String))

pop :: VmProgram Lir.Value
pop = do
  state <- get
  case stack state of
    (top : stack') -> do
      put $ state {stack = stack'}
      return top
    [] -> raiseErr $ "\n\tVM Stack Underflow!\n\tstate = " ++ show state ++ "\n"

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

popText :: VmProgram String
popText = do
  val <- pop
  let Lir.VText x = val
  return x

popInstrAddr :: VmProgram Lir.InstrAddr
popInstrAddr = do
  val <- pop
  case val of
    Lir.VInstrAddr x -> return x
    other -> raiseErr $ "Interal Vm Error: Expected VInstrAddr, got: " ++ show other

push :: Lir.Value -> VmProgram ()
push value = do
  state <- get
  put $ state {stack = value : stack state}

load :: String -> VmProgram ()
load var = do
  mem <- gets memory
  val <- findInFrames mem
  push val
  where
    findInFrames [] = raiseErr $ "Unbound variable" +++ codeIdent var
    findInFrames (frame : frames) =
      case M.lookup var frame of
        Just val -> return val
        Nothing -> findInFrames frames

store :: String -> VmProgram ()
store var = do
  val <- pop
  state <- get
  let (frame : parentFrames) = memory state
  let frame' = M.insert var val frame
  put $ state {memory = frame' : parentFrames}

setPc :: Lir.InstrAddr -> VmProgram ()
setPc pc' = do
  modify $ \state -> state {pc = pc'}

incrPc :: VmProgram ()
incrPc = do
  oldPc <- gets pc
  setPc (oldPc + 1)

pushRetAddr :: Lir.InstrAddr -> VmProgram ()
pushRetAddr retAddr = do
  retAddrs_ <- gets retAddrs
  modify $ \st -> st {retAddrs = retAddr : retAddrs_}

popRetAddr :: VmProgram Lir.InstrAddr
popRetAddr = do
  retAddrs_ <- gets retAddrs
  case retAddrs_ of
    retAddr : retAddrs' -> do
      modify $ \st -> st {retAddrs = retAddrs'}
      return retAddr
    [] -> raiseErr "Vm Return Stack Underflow!"

pushNewFrame :: VmProgram ()
pushNewFrame = do
  oldMem <- gets memory
  let newMem = M.empty : oldMem
  modify $ \state -> state {memory = newMem}

popMemFrame :: VmProgram ()
popMemFrame = do
  oldMem <- gets memory
  let (_ : newMem) = oldMem
  modify $ \state -> state {memory = newMem}

stepIntBinOp :: (Int -> Int -> Int) -> VmProgram ()
stepIntBinOp op = do
  x <- popInt
  y <- popInt
  push $ Lir.VInt $ x `op` y

stepSameTypeBoolBinOp :: (Lir.Value -> Lir.Value -> Bool) -> VmProgram ()
stepSameTypeBoolBinOp op = do
  x <- pop
  y <- pop
  push $ Lir.VBool $ x `op` y

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
      Lir.Pop -> do
        _ <- pop
        return ()
      Lir.Add -> stepIntBinOp (+)
      Lir.Sub -> stepIntBinOp (-)
      Lir.Mul -> stepIntBinOp (*)
      Lir.Div -> stepIntBinOp div
      Lir.Neg -> do
        i <- popInt
        push (Lir.VInt (- i))
      Lir.Gt -> stepIntBoolBinOp (>)
      Lir.Lt -> stepIntBoolBinOp (<)
      Lir.And -> stepBoolBinOp (&&)
      Lir.Or -> stepBoolBinOp (||)
      Lir.Not -> do
        b <- popBool
        push (Lir.VBool (not b))
      Lir.Concat -> do
        a <- popText
        b <- popText
        push $ Lir.VText $ a ++ b
      Lir.Jmp idx -> setPc (idx - 1)
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
        fnAddr <- popInstrAddr -- Get the function's entry address.
        callDirect fnAddr argC
      Lir.CallDirect fnAddr argC -> do
        callDirect fnAddr argC
      Lir.Ret -> do
        retAddr <- popRetAddr
        popMemFrame
        setPc (retAddr - 1)
        return ()
      Lir.Alloc n -> do
        idx <- heapAlloc n
        push $ Lir.VPtr idx

      -- BEFORE:
      -- TOS -> [val: Value]
      --        [buf_start: Value::VPtr]
      --        [...]
      -- AFTER:
      -- TOS -> [buf_start: Value::VPtr]
      --        [...]
      Lir.MemWriteDirect offset -> do
        val <- pop
        bufStart <- popPtr
        h <- gets heap
        let h' = M.insert (bufStart + offset) val h
        modify $ \st -> st {heap = h'}
        push $ Lir.VPtr bufStart
      -- Pops.
      Lir.MemReadDirect offset -> do
        bufStart <- popPtr
        h <- gets heap
        case M.lookup (bufStart + offset) h of
          Nothing -> raiseErr $ "MemReadDirect out of bounds addr:" +++ show (bufStart + offset)
          Just val -> push val
      -- Pops.
      Lir.TestDiscr discr -> do
        ptr <- popPtr
        h <- gets heap
        case M.lookup ptr h of
          Just (Lir.VInt d)
            | d == discr -> push $ Lir.VBool True
            | otherwise -> push $ Lir.VBool False
          x -> raiseErr $ "Expected VInt for discriminant, got" +++ code x

popPtr :: VmProgram Int
popPtr = do
  val <- pop
  case val of
    Lir.VPtr ptr -> return ptr
    x -> raiseErr $ "Expected VPtr, got" +++ code x

raiseErr ::
  (Monad (t1 (Either a1)), MonadTrans t2, MonadTrans t1) =>
  a1 ->
  t2 (t1 (Either a1)) a2
raiseErr msg = lift $ lift $ Left msg

heapAlloc :: Int -> VmProgram Int
heapAlloc slots = do
  h <- gets heap
  let idx = M.size h
  -- Extend and fill the new part with zeros.
  let h' = foldl (\h idx -> M.insert idx (Lir.VInt 0) h) h [idx .. idx + slots -1]
  modify $ \st -> st {heap = h'}
  return idx

callDirect :: Lir.InstrAddr -> Int -> VmProgram ()
callDirect fnAddr argC = do
  -- Store the return address above all the args.
  pc_ <- gets pc
  let retAddr = pc_ + 1 -- Return to the NEXT instr.
  pushRetAddr retAddr

  -- Perform the jump.
  setPc (fnAddr - 1)
  pushNewFrame
  return ()

runIntrinsic :: Intr.Intrinsic -> VmProgram ()
runIntrinsic op = case op of
  Intr.EqInt -> do
    x <- popInt
    y <- popInt
    push $ Lir.VBool $ x == y
  Intr.EqBool -> do
    x <- popBool
    y <- popBool
    push $ Lir.VBool $ x == y
  Intr.EqText -> do
    x <- popText
    y <- popText
    push $ Lir.VBool $ x == y
  Intr.DbgInt -> do
    x <- popInt
    lift $ tell $ Lir.dbgValue (Lir.VInt x) ++ "\n"
  Intr.DbgBool -> do
    x <- popBool
    lift $ tell $ Lir.dbgValue (Lir.VBool x) ++ "\n"
  Intr.DbgText -> do
    x <- popText
    lift $ tell $ Lir.dbgValue (Lir.VText x) ++ "\n"
  Intr.Puts -> do
    x <- popText
    lift $ tell $ x ++ "\n"
  Intr.Here pos -> do
    lift $ tell $ "intr.here[] at " ++ show pos ++ "\n"
  Intr.Exit -> do
    modify $ \state -> state {running = False}

runProgram :: VmProgram ()
runProgram = do
  (Lir.InstrAddr pc_) <- gets pc
  instrs_ <- gets instrs
  vmState <- get
  when (running vmState) $
    case M.lookup pc_ instrs_ of
      Just instr -> do
        stepVm instr
        runProgram
      Nothing -> raiseErr $ "VM: Illegal instruction address: " ++ show pc_

initState :: [Lir.Instr] -> VmState
initState instrs =
  VmState
    { memory = [M.empty],
      stack = [],
      heap = M.empty,
      pc = 0,
      retAddrs = [],
      instrs = M.fromList $ zip [0 ..] instrs,
      running = True
    }

execVmProgram :: [Lir.Instr] -> WriterT String (Either String) VmState
execVmProgram program = execStateT runProgram (initState program)

runAndCollectOutputFromVmProgram :: [Lir.Instr] -> Either String [String]
runAndCollectOutputFromVmProgram lir = do
  (_state, output) <- runWriterT $ execVmProgram lir
  return $ lines output
