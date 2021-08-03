def never-call-fn[] -> Never do
  (loop do end)[1, 2, 3];
end

def main[] do
  never-call-fn[];
  intr.puts["Done."];
end

# ===TAST===
# Mod({ main: [] -> Never, never-call-fn: [] -> Never }):
#   Item([] -> Never):
#     def never-call-fn[] -> Never (do
#       ((loop (do end : Void) : Never)[(1 : Int),(2 : Int),(3 : Int)] : Never)
#     end : Never)
#   Item([] -> Never):
#     def main[] -> Void (do
#       ((never-call-fn : [] -> Never)[] : Never);
#       (intr.puts[("Done." : Text)] : Void)
#     end : Never)


# ===HIR===
# Const (VLbl (Lbl 2))
# Store "main"
# Const (VLbl (Lbl 0))
# Store "never-call-fn"
# Const (VLbl (Lbl 2))
# Call 0
# Intrinsic Exit
# Label (Lbl 0)
# Const (VInt 1)
# Const (VInt 2)
# Const (VInt 3)
# Label (Lbl 1)
# Jmp (Lbl 1)
# Call 3
# Ret
# Label (Lbl 2)
# Load "never-call-fn"
# Call 0
# Const (VString "Done.")
# Intrinsic Print
# Ret


# ===LIR===
#   0: Const (VInstrAddr (InstrAddr 15))
#   1: Store "main"
#   2: Const (VInstrAddr (InstrAddr 7))
#   3: Store "never-call-fn"
#   4: Const (VInstrAddr (InstrAddr 15))
#   5: Call 0
#   6: Intrinsic Exit
#   7: Nop
#   8: Const (VInt 1)
#   9: Const (VInt 2)
#  10: Const (VInt 3)
#  11: Nop
#  12: Jmp (InstrAddr 11)
#  13: Call 3
#  14: Ret
#  15: Nop
#  16: Load "never-call-fn"
#  17: Call 0
#  18: Const (VString "Done.")
#  19: Intrinsic Pr