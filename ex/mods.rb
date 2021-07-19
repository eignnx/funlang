mod Foo
  def foo[x: Int] -> Int do
    x + 1
  end
end

def main[] do
  intr.print[foo[1]];

  def blah[] = intr.print["blah"]

  # blah[];
  nop
end