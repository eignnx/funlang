def main[] do
  let p = mk-pair[123, "abc"];
  intr.dbg-int[p.0];
  intr.dbg-text[p.1];
end

def mk-pair[a: Int, b: Text] -> Tuple[Int, Text] do
  {a, b}
end

# ===LIR===
#   0: CallDirect 11 0
#   1: Intrinsic Exit
#   2: Nop
#   3: Store b
#   4: Store a
#   5: Alloc 2
#   6: Load a
#   7: MemWriteDirect 0
#   8: Load b
#   9: MemWriteDirect 1
#  10: Ret
#  11: Nop
#  12: Const VInt 123
#  13: Const VText "abc"
#  14: CallDirect 2 2
#  15: Store p
#  16: Load p
#  17: MemReadDirect 0
#  18: Intrinsic DbgInt
#  19: Load p
#  20: MemReadDirect 1
#  21: Intrinsic DbgText
#  22: Ret