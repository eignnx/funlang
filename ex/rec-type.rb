type rec IntList
  | :Nil
  | :Cons Int, rec
end

def main[] do
  let x1 = {:Nil} as IntList;
  let x2 = {:Cons 1, {:Nil}} as IntList;
  let x3 = {:Cons 1, {:Cons 2, {:Nil}}} as IntList;
  # let x = map[double, {:Nil}];
  # map[double, {:Cons 1, {:Nil}}];
  # let xs = {:Cons 1, {:Cons 2, {:Nil}}} as rec IntList;
  # let ys = map[double, xs];
end

def map[f: [Int] -> Int, xs: rec IntList] -> rec IntList do
  match xs
    | { :Nil } => { :Nil }
    | { :Cons x, xs } => { :Cons f[x], map[f, xs] }
  end as rec IntList
end

def double[x: Int] = x + x