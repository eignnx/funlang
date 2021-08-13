type rec IntList
  | :Nil
  | :Cons Int, rec
end

def main[] do
  let xs = {:Cons 1, {:Cons 2, {:Nil}}};
  let ys = map[double, xs];
end

def map[f: [Int] -> Int, xs: rec IntList] -> rec IntList do
  match xs
    | { :Nil } => { :Nil }
    | { :Cons x, xs } => { :Cons f[x], map[f, xs] }
  end
end

def double[x: Int] = x + x