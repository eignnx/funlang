type NamedColor
  | :Red
  | :Green
  | :Blue
  | :Cyan
  | :Magenta
  | :Yellow
  | :Black
  | :White
end

type Color
  | :Rgb Int, Int, Int
  | :Hsb Int, Int, Int
  | NamedColor
end

def main[] do
  let green = { :Green } as NamedColor;
  let color = { :Rgb 50, 100, 200 } as Color;
  color = green; # Note: Should be allowed because NamedColor <: Color.

  match { :Rgb 4, 5, 6 } as Color #color
    | { :Red } => intr.puts["I love red!"]
    | { :Rgb r, g, b } =>
        intr.puts["The red component is"];   intr.dbg_int[r];
        intr.puts["The green component is"]; intr.dbg_int[g];
        intr.puts["The blue component is"];  intr.dbg_int[b];
    | other => intr.puts["I don't know that color!"]; # intr.dbg_int[b] # Should be scoping error!
  end
end

{#
===HIR===
CallDirect (Lbl 2) 0 :# "Call main"
Intrinsic Exit :# "Upon return from main, exit"
Label (Lbl 2) :# "Start of def main"
Alloc 1 :# "Allocate variant that has 0 fields"
Const (VInt 0) :# "Discriminant for { :Green }"
MemWriteDirect 0
Store "green"
Alloc 4 :# "Allocate variant that has 3 fields"
Const (VInt 1) :# "Discriminant for { :Rgb[$Int, $Int, $Int] }"
MemWriteDirect 0
Const (VInt 50)
MemWriteDirect 1 :# "Initialize the 0th variant field"
Const (VInt 100)
MemWriteDirect 2 :# "Initialize the 1th variant field"
Const (VInt 200)
MemWriteDirect 3 :# "Initialize the 2th variant field"
Store "color"
Load "green"
Store "color"
Alloc 4 :# "Allocate variant that has 3 fields"
Const (VInt 1) :# "Discriminant for { :Rgb[$Int, $Int, $Int] }"
MemWriteDirect 0
Const (VInt 4)
MemWriteDirect 1 :# "Initialize the 0th variant field"
Const (VInt 5)
MemWriteDirect 2 :# "Initialize the 1th variant field"
Const (VInt 6)
MemWriteDirect 3 :# "Initialize the 2th variant field"
Dup :# "Make a copy of scrutinee for next arm"
TestDiscr 2 :# "Discriminant for { :Red }"
JmpIfFalse (Lbl 4) :# "Jmp to next match arm if test fails"
Pop :# "Next arm will not be reached, pop its copy of scrutinee"
Const (VText "I love red!")
Intrinsic Puts
Jmp (Lbl 3) :# "Break out of match"
Label (Lbl 4) :# "Next match branch"
Dup :# "Make a copy of scrutinee for next arm"
TestDiscr 1 :# "Discriminant for { :Rgb[$Int, $Int, $Int] }"
JmpIfFalse (Lbl 5) :# "Jmp to next match arm if test fails"
Dup :# "We need a copy of local scrutinee from which to project"
MemReadDirect 1 :# "Project the 0th variant field"
Store "r"
Dup :# "We need a copy of local scrutinee from which to project"
MemReadDirect 2 :# "Project the 1th variant field"
Store "g"
Dup :# "We need a copy of local scrutinee from which to project"
MemReadDirect 3 :# "Project the 2th variant field"
Store "b"
Pop :# "Next arm will not be reached, pop its copy of scrutinee"
Const (VText "The red component is")
Intrinsic Puts
Load "r"
Intrinsic DbgInt
Const (VText "The green component is")
Intrinsic Puts
Load "g"
Intrinsic DbgInt
Const (VText "The blue component is")
Intrinsic Puts
Load "b"
Intrinsic DbgInt
Jmp (Lbl 3) :# "Break out of match"
Label (Lbl 5) :# "Next match branch"
Dup :# "Make a copy of scrutinee for next arm"
Store "other"
Pop :# "Next arm will not be reached, pop its copy of scrutinee"
Const (VText "I don't know that color!")
Intrinsic Puts
Jmp (Lbl 3) :# "Break out of match"
Label (Lbl 6) :# "Next match branch"
Label (Lbl 3) :# "Match end"
Ret :# "End of def main"
#}