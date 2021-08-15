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

### Longer Example
```ruby
type rec Expr
  | :Num Int
  | :Add rec, rec
  | :Mul rec, rec
  | :Var Text
  | :Let Text, rec, rec
end

type rec Ctx
  | :Empty
  | :Bind Text, Int, rec
end

def eval[ctx: rec Ctx, expr: rec Expr] -> Tuple[Ctx, Int] do
  match expr
    | { :Num x } => { ctx, x }
    | { :Add x, y } =>
        let {ctx, x} = eval[ctx, x];
        let {ctx, y} = eval[ctx, y];
        {ctx, x + y}
    | { :Mul x, y } =>
        let {ctx, x} = eval[ctx, x];
        let {ctx, y} = eval[ctx, y];
        {ctx, x * y}
    | { :Var x } => {ctx, ctx_lookup[ctx, x]}
    | { :Let x, expr, body } =>
        let {ctx, expr} = eval[ctx, expr];
        let ctx = { :Bind x, expr, ctx};
        eval[ctx, body]
  end as Tuple[rec Ctx, Int]
end

def ctx_lookup[ctx: Ctx, x: Text] -> Int do
  match ctx
    | { :Empty } =>
        intr.puts["unknown variable " ++ x];
        intr.exit[];
    | { :Bind y, value, ctx } =>
        if intr.eq_text[y, x] then
          value
        else 
          ctx_lookup[ctx, x]
        end
  end as Int
end

def main[] do
  let ctx = { :Empty };
  let expr =
    { :Let "x", { :Num 123 },
      { :Add { :Mul { :Num 2 }, { :Num 3 } }, { :Var "x" } } };
  let {ctx, result} = eval[ctx, expr];
  intr.dbg_int[result];
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
  - [X] Allow recursive types
