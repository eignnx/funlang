def f[] do
  let continue = true;
  while continue == true do
    "in f";
    continue = false;
  end
end

def main[] do
  "in main";
  let x = f[]; # Should be compilation error!
  intr.print[x]; # Should NOT print anything!
end

# ===LIR===
#   0: Const (VInstrAddr (InstrAddr 22))
#   1: Store "main"
#   2: Const (VInstrAddr (InstrAddr 7))
#   3: Store "f"
#   4: Const (VInstrAddr (InstrAddr 22))
#   5: Call 0
#   6: Intrinsic Exit
#   7: Nop
#   8: Const (VBool True)
#   9: Store "continue"
#  10: Nop
#  11: Const (VBool True)
#  12: Load "continue"
#  13: Eq
#  14: JmpIfFalse (InstrAddr 20)
#  15: Const (VString "in f")
#  16: Pop
#  17: Const (VBool False)
#  18: Store "continue"
#  19: Jmp (InstrAddr 10)
#  20: Nop
#  21: Ret
#  22: Nop
#  23: Const (VString "in main")
#  24: Pop
#  25: Load "f"
#  26: Call 0
#  27: Const (VString "<Void>")
#  28: Intrinsic Print
#  29: Ret