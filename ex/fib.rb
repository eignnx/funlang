# fib[n] = fib[n-1] + fib[n-2]

def main[] do
  n = 3;
  i = 1;
  j = 0;
  intr.here[];
  while n > 0 do
    nxt = i + j;
    j = i;
    i = nxt;
    intr.print[i];
    n = n - 1;
  end
  intr.here[];
  intr.print[i];
end