# This file tests the following property of well-typed code:
# 
# An expression that has multiple, sequentially-executed subexpressions MUST
# have type `Never` whenever ANY of it's sequentially-executed subexpressions
# has type `Never.`

def never_unary[] -> Never do
  not intr.exit[];
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