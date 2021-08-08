def disguised_as_never[] -> Never do
  intr.puts["Exiting..."];
  intr.exit[];
  # ret false
end

def main[] do
  let x = 1;
  x = disguised_as_never[];
  intr.puts[x]; # TODO: this should be a compilation error, right?
  intr.puts["Done."];
end