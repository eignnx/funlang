def str-join[a: Text, b: Text] -> Text do
  ret id[a ++ b]
end

def greet[name: Text] = say2["Hello ", name]

def say2[thing1: Text, thing2: Text] -> Unit do
  intr.print[str-join[thing1, thing2]];
end

def count[] do
  let n = 1;
  while true do
    intr.print[n];
    n = n + 1;
  end
end

def main[] do
  let name = "Quincy"; # Local variable.
  greet["Digger"];
  greet["Pippen"];
  intr.print[name] # Should print "Quincy"
end

def id[x: Text] = x

# ===LIR===
#   0: Const (VInstrAddr (InstrAddr 59))
#   1: Store "main"
#   2: Const (VInstrAddr (InstrAddr 44))
#   3: Store "count"
#   4: Const (VInstrAddr (InstrAddr 35))
#   5: Store "str-join"
#   6: Const (VInstrAddr (InstrAddr 31))
#   7: Store "id"
#   8: Const (VInstrAddr (InstrAddr 22))
#   9: Store "say2"
#  10: Const (VInstrAddr (InstrAddr 15))
#  11: Store "greet"
#  12: Const (VInstrAddr (InstrAddr 59))
#  13: Call 0
#  14: Intrinsic Exit
#  15: Nop
#  16: Store "name"
#  17: Const (VString "Hello ")
#  18: Load "name"
#  19: Load "say2"
#  20: Call 2
#  21: Ret
#  22: Nop
#  23: Store "thing2"
#  24: Store "thing1"
#  25: Load "thing1"
#  26: Load "thing2"
#  27: Load "str-join"
#  28: Call 2
#  29: Intrinsic Print
#  30: Ret
#  31: Nop
#  32: Store "x"
#  33: Load "x"
#  34: Ret
#  35: Nop
#  36: Store "b"
#  37: Store "a"
#  38: Load "b"
#  39: Load "a"
#  40: Concat
#  41: Load "id"
#  42: Call 1
#  43: Ret
#  44: Nop
#  45: Const (VInt 1)
#  46: Store "n"
#  47: Nop
#  48: Const (VBool True)
#  49: JmpIfFalse (InstrAddr 57)
#  50: Load "n"
#  51: Intrinsic Print
#  52: Const (VInt 1)
#  53: Load "n"
#  54: Add
#  55: Store "n"
#  56: Jmp (InstrAddr 47)
#  57: Nop
#  58: Ret
#  59: Nop
#  60: Const (VString "Quincy")
#  61: Store "name"
#  62: Const (VString "Digger")
#  63: Load "greet"
#  64: Call 1
#  65: Const (VString "Pippen")
#  66: Load "greet"
#  67: Call 1
#  68: Load "name"
#  69: Intrinsic Print
#  70: Ret