# fib[n] = fib[n-1] + fib[n-2]

def fib[n: Int] -> Int do
  let i = 1;
  let j = 0;
  while n > 1 do
    let nxt = i + j;
    j = i;
    i = nxt;
    n = n - 1;
  end
  i
end

def main[] do
  let n = 20;
  intr.puts["Working..."];
  intr.dbg_int[fib[n]];
  intr.puts["Done!"];
end