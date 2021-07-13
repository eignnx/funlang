def f[] do
  let continue = true;
  while continue == true do
    "in f";
    continue = false;
  end
end

def main[] do
  "in main";
  let x = f[]; # Should be compilation error!
  intr.print[x]; # Should NOT print anything!
end