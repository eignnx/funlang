type rec IntList
  | :Nil
  | :Cons Int, rec
end

def main[] do
  let x1 = {:Nil} as IntList;
  let x2 = {:Cons 1, {:Nil}} as IntList;
  let x3 = {:Cons 1, {:Cons 2, {:Nil}}} as IntList;
  let y3 = map[double, x3];
  dbg_int_list[y3];
end

def map[f: [Int] -> Int, xs: rec IntList] -> rec IntList do
  match xs
    | { :Nil } => { :Nil }
    | { :Cons x, xs } => { :Cons f[x], map[f, xs] }
  end as rec IntList
end

def dbg_int_list[xs: rec IntList] do
  intr.here[];
  match xs
    | { :Nil } => nop;
    | { :Cons x, xs } =>
        intr.dbg_int[x];
        dbg_int_list[xs];
  end as Void;
  intr.here[];
end

def double[x: Int] = x + x

{#
===HIR===
  CallDirect (Lbl 1) 0 :# "Call main"
  Intrinsic Exit :# "Upon return from main, exit"
Label (Lbl 4) :# "Start of def double"
  Store "x"
  Load "x"
  Load "x"
  Add
  Ret :# "End of def double"

Label (Lbl 3) :# "Start of def dbg_int_list"
  Store "xs"
  Load "xs"
  Dup :# "Make a copy of scrutinee for next arm"
  TestDiscr 0 :# "Discriminant for { :Nil }"
  JmpIfFalse (Lbl 9) :# "Jmp to next match arm if test fails"
  Pop :# "Next arm will not be reached, pop its copy of scrutinee"
  Nop :# "From `nop` expr"
  Jmp (Lbl 8) :# "Break out of match"
Label (Lbl 9) :# "Next match branch"
  Dup :# "Make a copy of scrutinee for next arm"
  TestDiscr 3 :# "Discriminant for { :Cons[$Int, { :Cons $Int, ?IntList | :Nil  }] }"
  JmpIfFalse (Lbl 10) :# "Jmp to next match arm if test fails"
  Dup :# "We need a copy of local scrutinee from which to project"
  MemReadDirect 1 :# "Project the 0th variant field"
  Store "x"
  Dup :# "We need a copy of local scrutinee from which to project"
  MemReadDirect 2 :# "Project the 1th variant field"
  Store "xs"
  Pop :# "Next arm will not be reached, pop its copy of scrutinee"
  Load "x"
  Intrinsic DbgInt
  Load "xs"
  CallDirect (Lbl 3) 1
  Jmp (Lbl 8) :# "Break out of match"
Label (Lbl 10) :# "Next match branch"
Label (Lbl 8) :# "Match end"
  Ret :# "End of def dbg_int_list"

Label (Lbl 2) :# "Start of def map"
  Store "xs"
  Store "f"
  Load "xs"
  Dup :# "Make a copy of scrutinee for next arm"
  TestDiscr 0 :# "Discriminant for { :Nil }"
  JmpIfFalse (Lbl 6) :# "Jmp to next match arm if test fails"
  Pop :# "Next arm will not be reached, pop its copy of scrutinee"
  Alloc 1 :# "Allocate variant that has 0 fields"
  Const (VInt 0) :# "Discriminant for { :Nil }"
  MemWriteDirect 0
  Jmp (Lbl 5) :# "Break out of match"
Label (Lbl 6) :# "Next match branch"
  Dup :# "Make a copy of scrutinee for next arm"
  TestDiscr 3 :# "Discriminant for { :Cons[$Int, { :Cons $Int, ?IntList | :Nil  }] }"
  JmpIfFalse (Lbl 7) :# "Jmp to next match arm if test fails"
  Dup :# "We need a copy of local scrutinee from which to project"
  MemReadDirect 1 :# "Project the 0th variant field"
  Store "x"
  Dup :# "We need a copy of local scrutinee from which to project"
  MemReadDirect 2 :# "Project the 1th variant field"
  Store "xs"
  Pop :# "Next arm will not be reached, pop its copy of scrutinee"
  Alloc 3 :# "Allocate variant that has 2 fields"
  Const (VInt 4) :# "Discriminant for { :Cons[$Int, rec ?IntList { { :Cons $Int, ?IntList | :Nil  } }] }"
  MemWriteDirect 0
  Load "x"
  Load "f"
  Call 1
  MemWriteDirect 1 :# "Initialize the 0th variant field"
  Load "f"
  Load "xs"
  CallDirect (Lbl 2) 2
  MemWriteDirect 2 :# "Initialize the 1th variant field"
  Jmp (Lbl 5) :# "Break out of match"
Label (Lbl 7) :# "Next match branch"
Label (Lbl 5) :# "Match end"
  Ret :# "End of def map"

Label (Lbl 1) :# "Start of def main"
  Alloc 1 :# "Allocate variant that has 0 fields"
  Const (VInt 0) :# "Discriminant for { :Nil }"
  MemWriteDirect 0
  Store "x1"
  Alloc 3 :# "Allocate variant that has 2 fields"
  Const (VInt 1) :# "Discriminant for { :Cons[$Int, { :Nil  }] }"
  MemWriteDirect 0
  Const (VInt 1)
  MemWriteDirect 1 :# "Initialize the 0th variant field"
  Alloc 1 :# "Allocate variant that has 0 fields"
  Const (VInt 0) :# "Discriminant for { :Nil }"
  MemWriteDirect 0
  MemWriteDirect 2 :# "Initialize the 1th variant field"
  Store "x2"
  Alloc 3 :# "Allocate variant that has 2 fields"
  Const (VInt 2) :# "Discriminant for { :Cons[$Int, { :Cons $Int, { :Nil  } }] }"
  MemWriteDirect 0
  Const (VInt 1)
  MemWriteDirect 1 :# "Initialize the 0th variant field"
  Alloc 3 :# "Allocate variant that has 2 fields"
  Const (VInt 1) :# "Discriminant for { :Cons[$Int, { :Nil  }] }"
  MemWriteDirect 0
  Const (VInt 2)
  MemWriteDirect 1 :# "Initialize the 0th variant field"
  Alloc 1 :# "Allocate variant that has 0 fields"
  Const (VInt 0) :# "Discriminant for { :Nil }"
  MemWriteDirect 0
  MemWriteDirect 2 :# "Initialize the 1th variant field"
  MemWriteDirect 2 :# "Initialize the 1th variant field"
  Store "x3"
  Const (VLbl (Lbl 4))
  Load "x3"
  CallDirect (Lbl 2) 2
  Store "y3"
  Load "y3"
  CallDirect (Lbl 3) 1
  Ret :# "End of def main"
#}