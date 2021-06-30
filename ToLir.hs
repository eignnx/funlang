module ToLir
  ( hirToLir
  )
where

import qualified Hir
import qualified Lir

import qualified Data.Map.Strict               as M

-------------------------TRANSLATING FROM Hir.Instr------------------

hirToLir :: [Hir.Instr] -> [Lir.Instr]
hirToLir instrs = map (translateInstr labels) instrs
  where labels = findLbls instrs

-------------------------FIRST PASS--------------------------------
type LblMap = M.Map Hir.Lbl Lir.InstrAddr

findLbls :: [Hir.Instr] -> LblMap
findLbls instrs = go instrs 0 M.empty
 where
  go :: [Hir.Instr] -> Lir.InstrAddr -> LblMap -> LblMap
  go []               _        labels = labels
  go (instr : instrs) instrIdx labels = case instr of
    Hir.Label lbl -> go instrs (instrIdx + 1) labels'
      where labels' = M.insert lbl instrIdx labels
    _ -> go instrs (instrIdx + 1) labels

-------------------------SECOND PASS-------------------------------

--
-- Does the translation given a complete map of labels-to-instrIdx's. 
--
translateInstr :: LblMap -> Hir.Instr -> Lir.Instr
translateInstr labels instr = case instr of
  Hir.Load  var       -> Lir.Load var
  Hir.Store var       -> Lir.Store var
  Hir.Const val       -> Lir.Const $ translateValue labels val
  Hir.Dup             -> Lir.Dup
  Hir.Over            -> Lir.Over
  Hir.Rot             -> Lir.Rot
  Hir.Add             -> Lir.Add
  Hir.Sub             -> Lir.Sub
  Hir.Mul             -> Lir.Mul
  Hir.Div             -> Lir.Div
  Hir.Neg             -> Lir.Neg
  Hir.And             -> Lir.And
  Hir.Or              -> Lir.Or
  Hir.Not             -> Lir.Not
  Hir.Eq              -> Lir.Eq
  Hir.Gt              -> Lir.Gt
  Hir.Lt              -> Lir.Lt
  Hir.Label      lbl  -> Lir.Nop -- Labels are translated to no-ops.
  Hir.JmpIfFalse lbl  -> translateJmp lbl Lir.JmpIfFalse labels
  Hir.Jmp        lbl  -> translateJmp lbl Lir.Jmp labels
  Hir.Intrinsic  intr -> Lir.Intrinsic intr

translateValue :: LblMap -> Hir.Value -> Lir.Value
translateValue labels hirVal =
  case hirVal of
    Hir.VInt x -> Lir.VInt x
    Hir.VBool x -> Lir.VBool x
    Hir.VString x -> Lir.VString x
    Hir.VLbl lbl ->
      case M.lookup lbl labels of
        Just instrAddr -> Lir.VInstrAddr instrAddr
        Nothing -> error $ "Internal Compilation Error: Unknown label: " ++ show lbl

translateJmp :: Hir.Lbl
             -> (Lir.InstrAddr -> Lir.Instr)
             -> LblMap
             -> Lir.Instr
translateJmp lbl jmpConstructor labels =
  case M.lookup lbl labels of
    Just idx -> jmpConstructor idx
    Nothing ->
      error $ "Internal Compilation Error: Unknown label: " ++ show lbl

-------------------------------------------------------------------