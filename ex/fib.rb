# fib[n] = fib[n-1] + fib[n-2]

def fib-iter[n: Int] do
  let i = 1;
  let j = 0;
  intr.here[];
  while n > 0 do
    let nxt = i + j;
    j = i;
    i = nxt;
    intr.print[i];
    n = n - 1;
  end
  intr.here[];
  intr.print[i];
end

def fib-rec[n: Int] -> Int do
  if n < 2 then
    1
  else
    fib-rec[n - 2] + fib-rec[n - 1]
  end
end

def main[] do
  fib-iter[10];
  intr.print[fib-rec[10]];
end