{-# LANGUAGE LambdaCase #-}

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
  where
    indices = [Lir.InstrAddr x | x <- [0..]]
    labels = findLbls instrs

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
    Hir.Comment instr _ -> go (instr:instrs) instrIdx labels
    _ -> go instrs (instrIdx + 1) labels

-------------------------SECOND PASS-------------------------------

--
-- Does the translation given a complete map of labels-to-instrIdx's. 
--
translateInstr :: LblMap -> Hir.Instr -> Lir.Instr
translateInstr labels instr = case instr of
  Hir.Load  var           -> Lir.Load var
  Hir.Store var           -> Lir.Store var
  Hir.Const val           -> Lir.Const $ translateValue labels val
  Hir.Dup                 -> Lir.Dup
  Hir.Over                -> Lir.Over
  Hir.Rot                 -> Lir.Rot
  Hir.Swap                -> Lir.Swap
  Hir.Pop                 -> Lir.Pop
  Hir.Add                 -> Lir.Add
  Hir.Sub                 -> Lir.Sub
  Hir.Mul                 -> Lir.Mul
  Hir.Div                 -> Lir.Div
  Hir.Neg                 -> Lir.Neg
  Hir.And                 -> Lir.And
  Hir.Or                  -> Lir.Or
  Hir.Not                 -> Lir.Not
  Hir.Gt                  -> Lir.Gt
  Hir.Lt                  -> Lir.Lt
  Hir.Concat              -> Lir.Concat
  Hir.Alloc n             -> Lir.Alloc n
  Hir.MemWriteDirect idx  -> Lir.MemWriteDirect idx
  Hir.MemReadDirect  idx  -> Lir.MemReadDirect idx
  Hir.Label      lbl      -> Lir.Nop -- Labels are translated to no-ops.
  Hir.JmpIfFalse lbl      -> Lir.JmpIfFalse $ translateLbl labels lbl
  Hir.Jmp        lbl      -> Lir.Jmp $ translateLbl labels lbl
  Hir.Intrinsic  intr     -> Lir.Intrinsic intr
  Hir.Call       argC     -> Lir.Call argC
  Hir.CallDirect lbl argC -> Lir.CallDirect (translateLbl labels lbl) argC
  Hir.Ret                 -> Lir.Ret
  Hir.Nop                 -> Lir.Nop
  Hir.Comment instr _     -> translateInstr labels instr

translateLbl :: LblMap -> Hir.Lbl -> Lir.InstrAddr
translateLbl labels lbl =
  case M.lookup lbl labels of
    Just instrAddr -> instrAddr
    Nothing -> error $ "Internal Compilation Error: Unknown label: " ++ show lbl

translateValue :: LblMap
               -> Hir.Value
               -> Lir.Value
translateValue labels = \case
  Hir.VInt x   -> Lir.VInt x
  Hir.VBool x  -> Lir.VBool x
  Hir.VText x  -> Lir.VText x
  Hir.VLbl lbl -> Lir.VInstrAddr $ translateLbl labels lbl
  Hir.VPtr idx -> Lir.VPtr idx
