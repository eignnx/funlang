type OptInt
  | :Some Int
  | :None
end

def nat_sum_til[opt: OptInt] -> Int do
  let {:Some upper} = opt else
    intr.puts["Can't unwrap {:None}!"];
    ret -1;
  end

  let total = 0;
  while upper > 0 do
    total = total + upper;
    upper = upper - 1;
  end
  total
end

def main[] do
  intr.dbg_int[nat_sum_til[{ :Some 2 }]];
  # intr.dbg_int[nat_sum_til[{ :None }]];
end

{#
===HIR===
  CallDirect (Lbl 2) 0 :# "Call main"
  Intrinsic Exit :# "Upon return from main, exit"
Label (Lbl 2) :# "Start of def main"
  Alloc 2 :# "Allocate variant that has 1 fields"       
  Const (VInt 0) :# "Discriminant for { :Some/1 }"      
  MemWriteDirect 0
  Const (VInt 2)
  MemWriteDirect 1 :# "Initialize the 0th variant field"
  CallDirect (Lbl 1) 1
  Intrinsic DbgInt
  Ret :# "End of def main"
Label (Lbl 1) :# "Start of def nat_sum_til"
  Store "opt"
  Load "opt"
  Dup :# "We may need a copy of `expr` to project from after discr. test"
  TestDiscr 0 :# "Discriminant for { :Some/1 }"
  JmpIfFalse (Lbl 3) :# "Jmp to next match arm if test fails"     
  Dup :# "We need a copy of local scrutinee from which to project"
  MemReadDirect 1 :# "Project the 0th variant field"
  Store "upper"
  Pop :# "Exiting `let-else` alternative, pop extra copy of `expr`"
  Jmp (Lbl 4) :# "Jump past the `let-else` alternative"
Label (Lbl 3) :# "Begin `let-else` alternative"
  Pop :# "Entering `let-else` alternative, pop extra copy of `expr`"
  Const (VText "Can't unwrap {:None}!")
  Intrinsic Puts
  Const (VInt 1)
  Neg
  Ret
Label (Lbl 4) :# "End `let-else` alternative"
  Const (VInt 0)
  Store "total"
Label (Lbl 5) :# "Top of while loop"
  Const (VInt 0)
  Load "upper"
  Gt
  JmpIfFalse (Lbl 6) :# "Jmp to end ofwhile loop"
  Load "upper"
  Load "total"
  Add
  Store "total"
  Const (VInt 1)
  Load "upper"
  Sub
  Store "upper"
  Jmp (Lbl 5)
Label (Lbl 6) :# "End of while loop"
  Load "total"
  Ret :# "End of def nat_sum_til"
#}