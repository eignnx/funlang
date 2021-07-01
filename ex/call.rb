# Const (VInstrAddr (InstrAddr 25))
# Store "main"
# Const (VInstrAddr (InstrAddr 15))
# Store "say2"
# Const (VInstrAddr (InstrAddr 7))
# Store "greet"
# Jmp (InstrAddr 25)
# Nop
# Swap
# Store "name"
# Const (VString "Hello ")
# Load "name"
# Load "say2"
# Call 2
# Ret
# Nop
# Swap
# Store "thing2"
# Swap
# Store "thing1"
# Load "thing1"
# Intrinsic Print
# Load "thing2"
# Intrinsic Print
# Ret
# Nop
# Const (VString "Quincy")
# Store "name"
# Const (VString "Digger")
# Load "greet"
# Call 1
# Const (VString "Pippen")
# Load "greet"
# Call 1
# Load "name"
# Intrinsic Print
# Intrinsic Exit

def greet[name] do
  say2["Hello ", name];
end

def say2[thing1, thing2] do
  intr.print[thing1];
  intr.print[thing2];
end

def main[] do
  name = "Quincy"; # Local variable.
  greet["Digger"];
  greet["Pippen"];
  intr.print[name]; # Should print "Quincy"
end
