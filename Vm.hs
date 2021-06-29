module Vm where

import qualified Compile                       as Ir
import           Control.Monad                  ( unless )
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Foldable
import qualified Data.Map.Strict               as M

data Instr
  = Load String
  | Store String
  | Const Ir.Value -- Push an immediate value onto stack
  | Dup -- Stack operations
  | Over -- "
  | Rot -- "
  | Add
  | Sub
  | Mul
  | Div
  | Neg
  | And
  | Or
  | Not
  | Eq
  | Gt
  | Lt
  | Nop -- Used to replace labels
  | JmpIfFalse Int
  | Jmp Int
  | Intrinsic Ir.Intrinsic
  deriving (Show)

-------------------------TRANSLATING FROM Ir.Instr------------------

vmFromIr :: [Ir.Instr] -> [Vm.Instr]
vmFromIr instrs = map (translateInstr labels) instrs
  where labels = findLbls instrs

-------------------------FIRST PASS--------------------------------
type LblMap = M.Map Ir.Lbl Int

findLbls :: [Ir.Instr] -> LblMap
findLbls instrs = go instrs 0 M.empty
 where
  go :: [Ir.Instr] -> Int -> LblMap -> LblMap
  go []               _        labels = labels
  go (instr : instrs) instrIdx labels = case instr of
    Ir.Label lbl -> go instrs (instrIdx + 1) labels'
      where labels' = M.insert lbl instrIdx labels
    _ -> go instrs (instrIdx + 1) labels

-------------------------SECOND PASS-------------------------------

--
-- Does the translation given a complete map of labels-to-instrIdx's. 
--
translateInstr :: LblMap -> Ir.Instr -> Vm.Instr
translateInstr labels instr = case instr of
  Ir.Load  var       -> Vm.Load var
  Ir.Store var       -> Vm.Store var
  Ir.Const val       -> Vm.Const val
  Ir.Dup             -> Vm.Dup
  Ir.Over            -> Vm.Over
  Ir.Rot             -> Vm.Rot
  Ir.Add             -> Vm.Add
  Ir.Sub             -> Vm.Sub
  Ir.Mul             -> Vm.Mul
  Ir.Div             -> Vm.Div
  Ir.Neg             -> Vm.Neg
  Ir.And             -> Vm.And
  Ir.Or              -> Vm.Or
  Ir.Not             -> Vm.Not
  Ir.Eq              -> Vm.Eq
  Ir.Gt              -> Vm.Gt
  Ir.Lt              -> Vm.Lt
  Ir.Label      lbl  -> Vm.Nop -- Labels are translated to no-ops.
  Ir.JmpIfFalse lbl  -> translateJmp lbl Vm.JmpIfFalse labels
  Ir.Jmp        lbl  -> translateJmp lbl Vm.Jmp labels
  Ir.Intrinsic  intr -> Vm.Intrinsic intr

translateJmp :: Ir.Lbl -> (Int -> Vm.Instr) -> LblMap -> Vm.Instr
translateJmp lbl jmpConstructor labels = do
  let assocdIdx = M.lookup lbl labels
  case assocdIdx of
    Just idx -> do
      jmpConstructor idx
    Nothing ->
      error $ "Internal Compilation Error: Unknown label: " ++ show lbl

-------------------------------------------------------------------

type Stack = [Ir.Value]

type Memory = [M.Map String Ir.Value]

type Pc = Int

data VmState = VmState
  { memory :: Memory
  , stack  :: Stack
  , pc     :: Pc
  , instrs :: [Vm.Instr]
  }
  deriving Show

type VmProgram = StateT VmState IO

pop :: VmProgram Ir.Value
pop = do
  state <- get
  case stack state of
    (top : stack') -> do
      put $ state { stack = stack' }
      return top
    [] -> error $ "\n\tVM Stack Underflow!\n\tstate = " ++ show state ++ "\n"

popInt :: VmProgram Integer
popInt = do
  val <- pop
  let Ir.VInt x = val
  return x

popBool :: VmProgram Bool
popBool = do
  val <- pop
  let Ir.VBool x = val
  return x

push :: Ir.Value -> VmProgram ()
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

setPc :: Pc -> VmProgram ()
setPc pc' = do
  modify $ \state -> state { pc = pc' }

incrPc :: VmProgram ()
incrPc = do
  oldPc <- gets pc
  setPc (oldPc + 1)

stepIntBinOp :: (Integer -> Integer -> Integer) -> VmProgram ()
stepIntBinOp op = do
  x <- popInt
  y <- popInt
  push $ Ir.VInt $ x `op` y

stepIntBoolBinOp :: (Integer -> Integer -> Bool) -> VmProgram ()
stepIntBoolBinOp op = do
  x <- popInt
  y <- popInt
  push $ Ir.VBool $ x `op` y

stepBoolBinOp :: (Bool -> Bool -> Bool) -> VmProgram ()
stepBoolBinOp op = do
  x <- popBool
  y <- popBool
  push $ Ir.VBool $ x `op` y

stepVm :: Vm.Instr -> VmProgram ()
stepVm instr = do
  step
  incrPc
 where
  step = case instr of
    Vm.Load var -> do
      load var
    Vm.Store var -> do
      store var
    Vm.Const val -> do
      push val
    Vm.Dup -> do
      tmp <- pop
      push tmp
      push tmp
    -- [a, b, ...] -> [b, a, b, ...]
    Vm.Over -> do
      a <- pop
      b <- pop
      push b
      push a
      push b
    -- [a, b, c, ...] -> [c, a, b, ...]
    Vm.Rot -> do
      a <- pop
      b <- pop
      c <- pop
      push c
      push a
      push b
    Vm.Add -> stepIntBinOp (+)
    Vm.Sub -> stepIntBinOp (-)
    Vm.Mul -> stepIntBinOp (*)
    Vm.Div -> stepIntBinOp div
    Vm.Neg -> do
      i <- popInt
      push (Ir.VInt (-i))
    Vm.Eq  -> stepIntBoolBinOp (==)
    Vm.Gt  -> stepIntBoolBinOp (>)
    Vm.Lt  -> stepIntBoolBinOp (<)
    Vm.And -> stepBoolBinOp (&&)
    Vm.Or  -> stepBoolBinOp (||)
    Vm.Not -> do
      b <- popBool
      push (Ir.VBool (not b))
    Vm.Jmp        idx -> setPc (idx - 1)
    Vm.JmpIfFalse idx -> do
      b <- popBool
      unless b $
                 -- we always incr pc, so goto idx - 1
                 setPc (idx - 1)
    Vm.Nop -> do
      return ()
    Vm.Intrinsic intr -> runIntrinsic intr

runIntrinsic :: Ir.Intrinsic -> VmProgram ()
runIntrinsic op = case op of
  Ir.Print -> do
    x <- pop
    lift $ Ir.displayValue x
  Ir.Here pos -> do
    lift $ putStrLn ("intr.here[] at " ++ show pos)

testProgram =
  [ Vm.Const (Ir.VInt 0)
  , Vm.Store "x"
  , Vm.Load "x" -- <<<
  , Vm.Intrinsic Ir.Print
  , Vm.Load "x"
  , Vm.Const (Ir.VInt 1)
  , Vm.Add
  , Vm.Store "x"
  , Vm.Load "x"
  , Vm.Const (Ir.VInt 5)
  , Vm.Gt
  , Vm.JmpIfFalse 13
  , Vm.Jmp 2
  , Vm.Const (Ir.VBool True) -- <<<
  , Vm.Intrinsic Ir.Print
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

initState :: [Vm.Instr] -> VmState
initState instrs =
  VmState { memory = [M.empty], stack = [], pc = 0, instrs = instrs }

execVmProgram :: [Vm.Instr] -> IO VmState
execVmProgram program = execStateT runProgram (initState program)
