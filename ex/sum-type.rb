type Color
  | :Rgb Int, Int, Int
  | :Hsb Int, Int, Int
  | NamedColor
end

type NamedColor
  | :Red
  | :Green
  | :Blue
  | :Cyan
  | :Magenta
  | :Yellow
  | :Black
  | :White
end

def main[] do
  let green = { :Green };
  let color = { :Rbg 50, 100, 200 } is Color;
  color = green; # Note: Should be allowed because NamedColor <: Color.
end