def loop-forever[] -> Never do
  "pls pop me!";
  loop do
    intr.here[];
  end
  intr.print["Infinity: reached."];
end

def main[] do
  loop-forever[]
end

# ===TAST===
# Mod({ loop-forever: [] -> Never, main: [] -> Void }):
#   Item([] -> Never):
#     def loop-forever[] -> Void (do
#       ("pls pop me!" : Text);
#       (loop (do
#         (intr.here[] : Void);
#       end : Void) : Never)
#       (intr.print[("Infinity: reached." : Text)] : Void);
#     end : Never)
#   Item([] -> Void):
#     def main[] -> Void (do
#       ((loop-forever : [] -> Void)[] : Void)
#     end : Void)


# ===HIR===
# Const (VLbl (Lbl 2))
# Store "main"
# Const (VLbl (Lbl 0))
# Store "loop-forever"
# Const (VLbl (Lbl 2))
# Call 0
# Intrinsic Exit
# Label (Lbl 0)
# Const (VString "pls pop me!")
# Pop
# Label (Lbl 1)
# Intrinsic (Here ".\\ex\\never.rb" (line 4, column 5))
# Jmp (Lbl 1)
# Const (VString "Infinity: reached.")
# Intrinsic Print
# Ret
# Label (Lbl 2)
# Load "loop-forever"
# Call 0
# Ret


# ===LIR===
#   0: Const (VInstrAddr (InstrAddr 16))
#   1: Store "main"
#   2: Const (VInstrAddr (InstrAddr 7))
#   3: Store "loop-forever"
#   4: Const (VInstrAddr (InstrAddr 16))
#   5: Call 0
#   6: Intrinsic Exit
#   7: Nop
#   8: Const (VString "pls pop me!")
#   9: Pop
#  10: Nop
#  11: Intrinsic (Here ".\\ex\\never.rb" (line 4, column 5))
#  12: Jmp (InstrAddr 10)
#  13: Const (VString "Infinity: reached.")
#  14: Intrinsic Print
#  15: Ret
#  16: Nop
#  17: Load "loop-forever"
#  18: Call 0
#  19: Ret