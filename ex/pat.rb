def main[] do
  let a = 1;
  let {b, c} = {true, 999};
  let {w, x, {{y}, z}} = {0, 1, {{2}, 3}};
  assert[w, 0, "w is wrong!"];
  assert[x, 1, "x is wrong!"];
  assert[y, 2, "y is wrong!"];
  assert[z, 3, "z is wrong!"];
  intr.puts["All passed!"];
end

def assert[expected: Int, actual: Int, msg: Text] do
  if not intr.eq-int[expected, actual] then
    intr.puts[msg];
    intr.exit[];
  else
    nop;
    nop;
    nop
  end
end