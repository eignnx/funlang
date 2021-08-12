# Funlang

It's a fun language-side-project (not that I have a real project to work on tho).

## What it Looks Like
```ruby
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
  color = green; # Note: allowed because NamedColor <: Color.

  match color
    | { :Red } => intr.puts["I love red!"]
    | { :Rgb r, g, b } =>
        intr.puts["The red component is"];   intr.dbg_int[r];
        intr.puts["The green component is"]; intr.dbg_int[g];
        intr.puts["The blue component is"];  intr.dbg_int[b];
    | other => intr.puts["I don't know that color!"];
  end
end
```

## Features/TODO
- [X] Compile a (somewhat) high-level language down to bytecode that can be run on a virtual machine
    - [X] Implement basic operations, and control flow statements
    - [X] Implement function calls
    - [ ] Implement lexical scoping for blocks
    - [ ] Implement closures
- [ ] Add a bidirectional type-checking algorithm
  - [X] Implement non-polymorphic type checking
  - [ ] Implement polymorphic type checking
- [ ] Add algebraic data types
  - [X] Impl basic variant types
  - [X] Impl tuple types
  - [X] Impl record types (currently called Mod types)
    - [ ] Call them records instead
  - [ ] Allow recursive types
