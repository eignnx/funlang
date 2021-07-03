#   0: Const (VInstrAddr (InstrAddr 29))
#   1: Store "main"
#   2: Const (VInstrAddr (InstrAddr 18))
#   3: Store "say2"
#   4: Const (VInstrAddr (InstrAddr 9))
#   5: Store "greet"
#   6: Const (VInstrAddr (InstrAddr 29))
#   7: Call 0
#   8: Intrinsic Exit
#   9: Nop
#  10: Swap
#  11: Store "name"
#  12: Const (VString "Hello ")
#  13: Load "name"
#  14: Load "say2"
#  15: Call 2
#  16: Pop
#  17: Ret
#  18: Nop
#  19: Swap
#  20: Store "thing2"
#  21: Swap
#  22: Store "thing1"
#  23: Load "thing2"
#  24: Load "thing1"
#  25: Concat
#  26: Intrinsic Print
#  27: Pop
#  28: Ret
#  29: Nop
#  30: Const (VString "Quincy")
#  31: Store "name"
#  32: Const (VString "Digger")
#  33: Load "greet"
#  34: Call 1
#  35: Pop
#  36: Const (VString "Pippen")
#  37: Load "greet"
#  38: Call 1
#  39: Pop
#  40: Load "name"
#  41: Intrinsic Print
#  42: Pop
#  43: Intrinsic Exit

def greet[name] do
  say2["Hello ", name];
end

def say2[thing1, thing2] do
  intr.print[thing1 ++ thing2];
end

def main[] do
  let name = "Quincy"; # Local variable.
  greet["Digger"];
  greet["Pippen"];
  intr.print[name]; # Should print "Quincy"
end
