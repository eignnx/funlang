# This file tests the following property of well-typed code:
# 
# An expression that has multiple, sequentially-executed subexpressions MUST
# have type `Never` whenever ANY of it's sequentially-executed subexpressions
# has type `Never.`

def never_unary[] -> Never do
  (not (intr.exit[] as Never)) as Never;
end

def never_binary[] -> Never do
  1 + intr.exit[];
end

def foo[x: Int, y: Int, z: Int] do
end

def never_call_args[] -> Never do
  foo[1, intr.exit[], 3];
end

def never_call_fn[] -> Never do
  intr.exit[][1, 2, 3];
end

def never_intr[] -> Never do
  intr.puts[intr.exit[]];
end

def never_if_cond[] -> Never do
  if intr.exit[] then
    nop
  else
    nop
  end
end

def never_match_scrut[] -> Never do
  match intr.exit[]
    | {:Blah} => nop
    | {:Blergh} => nop
  end
end

def never_while_cond[] -> Never do
  while intr.exit[] do
    nop;
  end
end

def never_let[] -> Never do
  let x = intr.exit[];
end

def never_assign[] -> Never do
  let x = 1;
  x = intr.exit[];
end

def main[] = nop

{#
===HIR===
  CallDirect (Lbl 11) 0 :# "Call main"
  Intrinsic Exit :# "Upon return from main, exit"
Label (Lbl 11) :# "Start of def main"
  Nop :# "From `nop` expr"
  Ret :# "End of def main"
Label (Lbl 10) :# "Start of def never_assign"
  Const (VInt 1)
  Store "x"
  Intrinsic Exit
  Ret :# "End of def never_assign"
Label (Lbl 9) :# "Start of def never_let"
  Intrinsic Exit
  Ret :# "End of def never_let"
Label (Lbl 8) :# "Start of def never_while_cond"
Label (Lbl 14) :# "Top of while loop"
  Intrinsic Exit
  JmpIfFalse (Lbl 15) :# "Jmp to end ofwhile loop"
  Nop :# "From `nop` expr"
  Jmp (Lbl 14)
Label (Lbl 15) :# "End of while loop"
  Ret :# "End of def never_while_cond"
Label (Lbl 7) :# "Start of def never_match_scrut"
  Intrinsic Exit
  Ret :# "End of def never_match_scrut"
Label (Lbl 6) :# "Start of def never_if_cond"
  Intrinsic Exit
  JmpIfFalse (Lbl 12)
  Nop :# "From `nop` expr"
  Jmp (Lbl 13)
Label (Lbl 12) :# "Else branch"
  Nop :# "From `nop` expr"
Label (Lbl 13) :# "End of `if` expr"
  Ret :# "End of def never_if_cond"
Label (Lbl 5) :# "Start of def never_intr"
  Intrinsic Exit
  Intrinsic Puts
  Ret :# "End of def never_intr"
Label (Lbl 4) :# "Start of def never_call_fn"
  Const (VInt 1)
  Const (VInt 2)
  Const (VInt 3)
  Intrinsic Exit
  Call 3
  Ret :# "End of def never_call_fn"
Label (Lbl 3) :# "Start of def never_call_args"
  Const (VInt 1)
  Intrinsic Exit
  Const (VInt 3)
  CallDirect (Lbl 2) 3
  Ret :# "End of def never_call_args"
Label (Lbl 2) :# "Start of def foo"
  Store "z"
  Store "y"
  Store "x"
  Ret :# "End of def foo"
Label (Lbl 1) :# "Start of def never_binary"
  Intrinsic Exit
  Const (VInt 1)
  Add
  Ret :# "End of def never_binary"
Label (Lbl 0) :# "Start of def never_unary"
  Intrinsic Exit
  Not
  Ret :# "End of def never_unary"
#}