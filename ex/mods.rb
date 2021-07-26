mod Foo
  def foo[x: Int] -> Int do
    x + 1
  end

  let MyFavNum = 7 + -49 * 3 / 2
end

def main[] do
  intr.print[foo[MyFavNum]];

  def blah[] = intr.print["blah"]

  intr.print["vvvvv  Expecting 'blah' here  vvvvv"];
  blah[];
  intr.print["^^^^^ Did 'blah' get printed? ^^^^^"]
end