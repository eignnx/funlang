def count-forever[] -> Void do
  let n = 1;
  while true do
    intr.print[n];
    n = n + 1;
  end
end

def loop-forever[] -> Never do
  loop do
    intr.here[];
  end
end

def main[] do
  count-forever[]
end