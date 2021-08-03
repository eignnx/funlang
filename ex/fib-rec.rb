def fib[n: Int] -> Int do
  if n < 2 then
    n
  else
    fib[n - 2] + fib[n - 1]
  end
end

def main[] do
  let n = 20;
  intr.puts["Working..."];
  intr.dbg-int[fib[n]];
  intr.puts["Done!"];
end