def loop-forever[] -> Never do
  loop do
    intr.here[];
  end
  intr.print["Infinity: reached."];
end

def main[] do
  loop-forever[]
end