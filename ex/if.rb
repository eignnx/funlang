def main[] do
  let greeting = greet-digger["Digger"];
  intr.puts[greeting];
end

def greet-digger[name: Text] -> Text do
  if intr.eq-text[name, "Digger"] then
    "You're a good doggy!"
  else
    "Who are you?"
  end
end

#   0: Const (VInstrAddr (InstrAddr 15))
#   1: Store "greet-digger"
#   2: Const (VInstrAddr (InstrAddr 7))
#   3: Store "main"
#   4: Const (VInstrAddr (InstrAddr 7))
#   5: Call 0
#   6: Intrinsic Exit
#   7: Nop
#   8: Const (VText "Digger")
#   9: Load "greet-digger"
#  10: Call 1
#  11: Store "greeting"
#  12: Load "greeting"
#  13: Intrinsic Print
#  14: Ret
#  15: Nop
#  16: Store "name"
#  17: Const (VText "Digger")
#  18: Load "name"
#  19: Eq
#  20: JmpIfFalse (InstrAddr 23)
#  21: Const (VText "You're a good doggy!")
#  22: Jmp (InstrAddr 25)
#  23: Nop
#  24: Const (VText "Who are you?")
#  25: Nop
#  26: Ret
