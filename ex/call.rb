{#
  0: Const (VInstrAddr (InstrAddr 34))
  1: Store "main"
  2: Const (VInstrAddr (InstrAddr 25))
  3: Store "say2"
  4: Const (VInstrAddr (InstrAddr 18))
  5: Store "str-join"
  6: Const (VInstrAddr (InstrAddr 11))
  7: Store "greet"
  8: Const (VInstrAddr (InstrAddr 34))
  9: Call 0
 10: Intrinsic Exit
 11: Nop
 12: Store "name"
 13: Const (VString "Hello ")
 14: Load "name"
 15: Load "say2"
 16: Call 2
 17: Ret
 18: Nop
 19: Store "b"
 20: Store "a"
 21: Load "b"
 22: Load "a"
 23: Concat
 24: Ret
 25: Nop
 26: Store "thing2"
 27: Store "thing1"
 28: Load "thing1"
 29: Load "thing2"
 30: Load "str-join"
 31: Call 2
 32: Intrinsic Print
 33: Ret
 34: Nop
 35: Const (VString "Quincy")
 36: Store "name"
 37: Const (VString "Digger")
 38: Load "greet"
 39: Call 1
 40: Const (VString "Pippen")
 41: Load "greet"
 42: Call 1
 43: Load "name"
 44: Intrinsic Print
 45: Ret
#}
def greet[name] do
  say2["Hello ", name];
end

def str-join[a, b] do
  ret a ++ b;
end

def say2[thing1, thing2] do
  intr.print[str-join[thing1, thing2]];
end

def main[] do
  let name = "Quincy"; # Local variable.
  greet["Digger"];
  greet["Pippen"];
  intr.print[name] # Should print "Quincy"
end
