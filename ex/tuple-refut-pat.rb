def main[] do
  let {{:X}, {:X}} = {{:X}, {:X}} else
    intr.exit[];
  end
  intr.here[];
end

{#
0: CallDirect 2 0
1: Intrinsic Exit
2: Nop
3: Alloc 2
4: Alloc 1
5: Const VInt 0
6: MemWriteDirect 0
7: MemWriteDirect 0
8: Alloc 1
9: Const VInt 0
10: MemWriteDirect 0
11: MemWriteDirect 1
12: Dup
13: Dup
14: MemReadDirect 0
15: TestDiscr 0
16: JmpIfFalse 22
17: MemReadDirect 1
18: TestDiscr 0
19: JmpIfFalse 22
20: Pop
21: Jmp 25
22: Nop
23: Pop
24: Intrinsic Exit
25: Nop
26: Intrinsic Here "./ex/tuple-refut-pat.rb" (line 5, column 3)
27: Ret
#}