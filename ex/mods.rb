mod Foo
  def foo[x: Int] -> Int do
    x + 1
  end
end

def main[] do
  intr.print[foo[1]];

  def blah[] = intr.print["blah"]

  intr.print["vvvvv  Expecting 'blah' here  vvvvv"];
  blah[];
  intr.print["^^^^^ Did 'blah' get printed? ^^^^^"]
end