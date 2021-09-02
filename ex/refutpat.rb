{# ===HIR===; CallDirect (Lbl 1) 0 :# "Call main"; Intrinsic Exit :# "Upon return from main, exit"
Label (Lbl 1) :# "Start of def main"
  Alloc 2 :# "Allocate an 2-tuple"
  Const (VInt 123)
  MemWriteDirect 0 :# "Initialize the 0th tuple field of `{ (123 :<: $Int), ({ :Some[({ (456 :<: $Int), (\"blerg\" :<: $Text) } :<: Tuple[$Int, $Text])] } :<: { :Some Tuple[$Int, $Text] }) }`"
  Alloc 2 :# "Allocate variant that has 1 fields"
  Const (VInt 0) :# "Discriminant for { :Some/1 }"        
  MemWriteDirect 0
  Alloc 2 :# "Allocate an 2-tuple"
  Const (VInt 456)
  MemWriteDirect 0 :# "Initialize the 0th tuple field of `{ (456 :<: $Int), (\"blerg\" :<: $Text) }`"
  Const (VText "blerg")
  MemWriteDirect 1 :# "Initialize the 1th tuple field of `{ (456 :<: $Int), (\"blerg\" :<: $Text) }`"
  MemWriteDirect 1 :# "Initialize the 0th variant field of `{ :Some[({ (456 :<: $Int), (\"blerg\" :<: $Text) } :<: Tuple[$Int, $Text])] }`"
  MemWriteDirect 1 :# "Initialize the 1th tuple field of `{ (123 :<: $Int), ({ :Some[({ (456 :<: $Int), (\"blerg\" :<: $Text) } :<: Tuple[$Int, $Text])] } :<: { :Some Tuple[$Int, $Text] }) }`"
  Store "scrut"
  Load "scrut"
  Dup :# "We may need a copy of `expr` to project from after discr. test"
  Dup :# "Make a copy of scrutinee for all but the last tuple field"     
  MemReadDirect 0 :# "Project the 0th tuple field of `x`"
  Store "x"
  MemReadDirect 1 :# "Project the 1th tuple field of `{ :Some { y, z } }`"
  TestDiscr 0 :# "Discriminant for { :Some/1 }"
  JmpIfFalse (Lbl 2) :# "Jmp to next match arm if test fails"
  Dup :# "We need a copy of local scrutinee from which to project"
  MemReadDirect 1 :# "Project the 0th variant field of `{ :Some { y, z } }`"
  Dup :# "Make a copy of scrutinee for all but the last tuple field"
  MemReadDirect 0 :# "Project the 0th tuple field of `y`"
  Store "y"
  MemReadDirect 1 :# "Project the 1th tuple field of `z`"
  Store "z"
  Pop :# "Exiting `let-else` alternative, pop extra copy of `expr`"
  Jmp (Lbl 3) :# "Jump past the `let-else` alternative"
Label (Lbl 2) :# "Begin `let-else` alternative"
  Pop :# "Entering `let-else` alternative, pop extra copy of `expr`"
  Const (VText "Got :None, not :Some!")
  Intrinsic Puts
  Intrinsic Exit
Label (Lbl 3) :# "End `let-else` alternative"
  Const (VText "Got :Some!")
  Intrinsic Puts
  Load "x"
  Intrinsic DbgInt
  Load "y"
  Intrinsic DbgInt
  Load "z"
  Intrinsic DbgText
  Ret :# "End of def main"
#}

type PairOpt
  | :Some Tuple[Int, Text]
  | :None
end

def main[] do
  let scrut = { 123, { :Some { 456, "blerg" } } };
  # let scrut = { :Some { 456, "blerg" } };
  let { x, { :Some { y, z } } } = scrut else
  # let { :Some { y, z } } = scrut else
    intr.puts["Got :None, not :Some!"];
    intr.exit[];
  end
  intr.puts["Got :Some!"];
  intr.dbg_int[x];
  intr.dbg_int[y];
  intr.dbg_text[z];
  # match scrut as Tuple[Int, PairOpt]
  #   | { x, { :None } } => intr.puts["None."]
  #   | { x, { :Some { y, z } } } => intr.puts["Got it!"]
  # end
end
