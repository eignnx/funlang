mod Foo
  def foo[x: Int] -> Int do
    x + 1
  end

  static MyFavNum = 7 + -49 * 3 / if true then 2 else 3 end
  static MySecondFavNum = MyFavNum + 100
end

def main[] do
  intr.print[foo[MySecondFavNum]];

  def blah[] = intr.print["blah"]

  intr.print["vvvvv  Expecting 'blah' here  vvvvv"];
  blah[];
  intr.print["^^^^^ Did 'blah' get printed? ^^^^^"]
end