module Vm where

import qualified Compile                       as C
import           Control.Monad                  ( unless )
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Foldable
import qualified Data.Map.Strict               as M

data Instr
  = Load String
  | Store String
  | Const C.Value -- Push an immediate value onto stack
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
  | Intrinsic C.Intrinsic
  deriving (Show)

-------------------------TRANSLATING FROM C.Instr------------------

vmFromIr :: [C.Instr] -> [Vm.Instr]
vmFromIr instrs = map (translateInstr labels) instrs
  where labels = findLbls instrs

-------------------------FIRST PASS--------------------------------
type LblMap = M.Map C.Lbl Int

findLbls :: [C.Instr] -> LblMap
findLbls instrs = go instrs 0 M.empty
 where
  go :: [C.Instr] -> Int -> LblMap -> LblMap
  go []               _        labels = labels
  go (instr : instrs) instrIdx labels = case instr of
    C.Label lbl -> go instrs (instrIdx + 1) labels'
      where labels' = M.insert lbl instrIdx labels
    _ -> go instrs (instrIdx + 1) labels

-------------------------SECOND PASS-------------------------------

--
-- Does the translation given a complete map of labels-to-instrIdx's. 
--
translateInstr :: LblMap -> C.Instr -> Vm.Instr
translateInstr labels instr = case instr of
  C.Load  var       -> Vm.Load var
  C.Store var       -> Vm.Store var
  C.Const val       -> Vm.Const val
  C.Dup             -> Vm.Dup
  C.Over            -> Vm.Over
  C.Rot             -> Vm.Rot
  C.Add             -> Vm.Add
  C.Sub             -> Vm.Sub
  C.Mul             -> Vm.Mul
  C.Div             -> Vm.Div
  C.Neg             -> Vm.Neg
  C.And             -> Vm.And
  C.Or              -> Vm.Or
  C.Not             -> Vm.Not
  C.Eq              -> Vm.Eq
  C.Gt              -> Vm.Gt
  C.Lt              -> Vm.Lt
  C.Label      lbl  -> Vm.Nop -- Labels are translated to no-ops.
  C.JmpIfFalse lbl  -> translateJmp lbl Vm.JmpIfFalse labels
  C.Jmp        lbl  -> translateJmp lbl Vm.Jmp labels
  C.Intrinsic  intr -> Vm.Intrinsic intr

translateJmp :: C.Lbl -> (Int -> Vm.Instr) -> LblMap -> Vm.Instr
translateJmp lbl jmpConstructor labels = do
  let assocdIdx = M.lookup lbl labels
  case assocdIdx of
    Just idx -> do
      jmpConstructor idx
    Nothing ->
      error $ "Internal Compilation Error: Unknown label: " ++ show lbl

-------------------------------------------------------------------

type Stack = [C.Value]

type Memory = [M.Map String C.Value]

type Pc = Int

data VmState = VmState
  { memory :: Memory
  , stack  :: Stack
  , pc     :: Pc
  }
  deriving Show

type VmProgram = StateT VmState IO

pop :: VmProgram C.Value
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
  let C.VInt x = val
  return x

popBool :: VmProgram Bool
popBool = do
  val <- pop
  let C.VBool x = val
  return x

push :: C.Value -> VmProgram ()
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
  push $ C.VInt $ x `op` y

stepIntBoolBinOp :: (Integer -> Integer -> Bool) -> VmProgram ()
stepIntBoolBinOp op = do
  x <- popInt
  y <- popInt
  push $ C.VBool $ x `op` y

stepBoolBinOp :: (Bool -> Bool -> Bool) -> VmProgram ()
stepBoolBinOp op = do
  x <- popBool
  y <- popBool
  push $ C.VBool $ x `op` y

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
      push (C.VInt (-i))
    Vm.Eq  -> stepIntBoolBinOp (==)
    Vm.Gt  -> stepIntBoolBinOp (>)
    Vm.Lt  -> stepIntBoolBinOp (<)
    Vm.And -> stepBoolBinOp (&&)
    Vm.Or  -> stepBoolBinOp (||)
    Vm.Not -> do
      b <- popBool
      push (C.VBool (not b))
    Vm.Jmp        idx -> setPc (idx - 1)
    Vm.JmpIfFalse idx -> do
      b <- popBool
      unless b $
                 -- we always incr pc, so goto idx - 1
                 setPc (idx - 1)
    Vm.Nop -> do
      return ()
    Vm.Intrinsic intr -> runIntrinsic intr

runIntrinsic :: C.Intrinsic -> VmProgram ()
runIntrinsic op = case op of
  C.Print -> do
    x <- pop
    lift $ print x
  C.Here pos -> do
    lift $ putStrLn ("@here[] at " ++ show pos)

testProgram =
  [ Vm.Const (C.VInt 0)
  , Vm.Store "x"
  , Vm.Load "x" -- <<<
  , Vm.Intrinsic C.Print
  , Vm.Load "x"
  , Vm.Const (C.VInt 1)
  , Vm.Add
  , Vm.Store "x"
  , Vm.Load "x"
  , Vm.Const (C.VInt 5)
  , Vm.Gt
  , Vm.JmpIfFalse 13
  , Vm.Jmp 2
  , Vm.Const (C.VBool True) -- <<<
  , Vm.Intrinsic C.Print
  ]

nth :: (Ord i, Num i) => [a] -> i -> Maybe a
nth _ n | n < 0 = Nothing
nth []       n  = Nothing
nth (x : xs) 0  = Just x
nth (_ : xs) n  = nth xs (n - 1)

debugStepProgram :: [Vm.Instr] -> VmProgram ()
debugStepProgram instrs = do
  pc1 <- gets pc
  st  <- get
  lift $ putChar '\t'
  lift $ print st
  lift $ putStr "\texecuting instr: "
  lift $ print (nth instrs pc1)
  Data.Foldable.mapM_ stepVm (nth instrs pc1)

runProgram :: [Vm.Instr] -> VmProgram ()
runProgram instrs = do
  pc1 <- gets pc
  case nth instrs pc1 of
    Just instr -> stepVm instr >> runProgram instrs
    Nothing    -> return ()

initState :: VmState
initState = VmState { memory = [M.empty], stack = [], pc = 0 }

execVmProgram :: [Instr] -> IO VmState
execVmProgram program = execStateT (runProgram program) initState
