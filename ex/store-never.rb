def text-disguised-as-never[] -> Never do
  intr.exit[];
  # ret false
end

def main[] do
  let x = 1;
  x = text-disguised-as-never[];
  intr.puts[x];
end