def main[] do
  let p = mk_pair[123, "abc"];
  intr.dbg_int[p.0];
  intr.dbg_text[p.1.2];
end

def mk_pair[a: Int, b: Text] -> Tuple[Int, Tuple[Int, Bool, Text]] do
  {a, {a, false, b}}
end

# ===LIR===
#   0: CallDirect 17 0
#   1: Intrinsic Exit
#   2: Nop
#   3: Store b
#   4: Store a
#   5: Alloc 2
#   6: Load a
#   7: MemWriteDirect 0
#   8: Alloc 3
#   9: Load a
#  10: MemWriteDirect 0
#  11: Const VBool False
#  12: MemWriteDirect 1
#  13: Load b
#  14: MemWriteDirect 2
#  15: MemWriteDirect 1
#  16: Ret
#  17: Nop
#  18: Const VInt 123
#  19: Const VText "abc"
#  20: CallDirect 2 2
#  21: Store p
#  22: Load p
#  23: MemReadDirect 0
#  24: Intrinsic DbgInt
#  25: Load p
#  26: MemReadDirect 1
#  27: MemReadDirect 2
#  28: Intrinsic DbgText
#  29: Ret