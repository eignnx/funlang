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

type Color
  | :Rgb Int, Int, Int
  | :Hsb Int, Int, Int
  | NamedColor
end

def main[] do
  let green = { :Green } as NamedColor;
  let color = { :Rgb 50, 100, 200 } as Color;
  color = green; # Note: Should be allowed because NamedColor <: Color.

  match color
    | { :Red } => intr.puts["I love red!"]
    | { :Rgb r, g, b } =>
        intr.puts["The red component is"];   intr.dbg_int[r];
        intr.puts["The green component is"]; intr.dbg_int[g];
        intr.puts["The blue component is"];  intr.dbg_int[b];
    | other => intr.puts["I don't know that color!"]; # intr.dbg_int[b] # Should be scoping error!
  end
end