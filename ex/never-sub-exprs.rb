# This file tests the following property of well-typed code:
# 
# An expression that has multiple, sequentially-executed subexpressions MUST
# have type `Never` whenever ANY of it's sequentially-executed subexpressions
# has type `Never.`

def never-unary[] -> Never do
  not intr.exit[];
end

def never-binary[] -> Never do
  1 + intr.exit[];
end

# Operator `==` is kinda a special case.
def never-eq[] -> Never do
  if "blah" == intr.exit[] then
    1
  else
    2
  end
end

def foo[x: Int, y: Int, z: Int] do
end

def never-call-args[] -> Never do
  foo[1, intr.exit[], 3];
end

def never-call-fn[] -> Never do
  intr.exit[][1, 2, 3];
end

def never-intr[] -> Never do
  intr.print[intr.exit[]];
end

def never-if-cond[] -> Never do
  if intr.exit[] then
    nop
  else
    nop
  end
end

def never-while-cond[] -> Never do
  while intr.exit[] do
    nop;
  end
end

def never-let[] -> Never do
  let x = intr.exit[];
end

def never-assign[] -> Never do
  let x = 1;
  x = intr.exit[];
end

def main[] = nop