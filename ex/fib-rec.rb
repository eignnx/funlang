def fib[n: Int] -> Int do
  if n < 2 then
    n
  else
    fib[n - 2] + fib[n - 1]
  end
end

def main[] do
  let n = 20;
  intr.print["Working..."];
  intr.print[fib[n]];
  intr.print["Done!"];
end