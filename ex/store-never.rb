def disguised_as_never[] -> Never do
  intr.exit[];
  # ret false
end

def main[] do
  let x = 1;
  x = disguised_as_never[];
  intr.puts[x];
end