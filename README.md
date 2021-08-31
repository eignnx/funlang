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
  | :Bool Bool
  | :Add rec, rec
  | :Mul rec, rec
  | :If rec, rec, rec
  | :Var Text
  | :Let Text, rec, rec
end

# Note: `Val` and `Expr` are allowed to have overlapping constructor names!
type Val
  | :Num Int
  | :Bool Bool
end

type rec Ctx
  | :Empty
  | :Bind Text, Val, rec
end

def eval[ctx: Ctx, expr: Expr] -> Tuple[Ctx, Val] do
  match expr
    | { :Num x } => { ctx, { :Num x } }
    | { :Bool x } => { ctx, { :Bool x } }
    | { :Add x, y } =>
        let { ctx, x } = eval[ctx, x];
        let { ctx, y } = eval[ctx, y];
        let { :Num x } = x else
          type_error["The `+` operator requires numbers!"];
        end
        let { :Num y } = y else
          type_error["The `+` operator requires numbers!"];
        end
        { ctx, { :Num x + y } }
    | { :Mul x, y } =>
        let { ctx, x } = eval[ctx, x];
        let { ctx, y } = eval[ctx, y];
        let { :Num x } = x else
          type_error["The `*` operator requires numbers!"];
        end
        let { :Num y } = y else
          type_error["The `*` operator requires numbers!"];
        end
        { ctx, { :Num x * y } }
    | { :If cond, yes, no } =>
        let { ctx, cond } = eval[ctx, cond];
        let { :Bool cond } = cond else
          type_error["An `if` expression requires a boolean in it's conditional!"];
        end
        if cond then
          eval[ctx, yes]
        else
          eval[ctx, no]
        end
    | { :Var x } => { ctx, ctx_lookup[ctx, x] }
    | { :Let x, expr, body } =>
        let { ctx, val } = eval[ctx, expr];
        let ctx = { :Bind x, val, ctx };
        eval[ctx, body]
  end
end

def ctx_lookup[ctx: Ctx, x: Text] -> Val do
  match ctx
    | { :Empty } =>
        intr.puts["Unbound variable " ++ x];
        intr.exit[];
    | { :Bind y, val, ctx } =>
        if intr.eq_text[y, x] then
          val
        else 
          ctx_lookup[ctx, x]
        end
  end
end

def type_error[msg: Text] -> Never do
  intr.puts["Type Error: " ++ msg];
  intr.exit[];
end

def print_val[val: Val] do
  match val
    | { :Num x } => intr.dbg_int[x]
    | { :Bool x } => intr.dbg_bool[x]
    end
end

def main[] do
  let ctx = { :Empty };
  let expr =
    { :Let "x", { :Num 123 },
      { :Add { :Mul { :Num 2 }, { :Num 3 } }, { :Var "x" } } };
  let { ctx, result } = eval[ctx, expr];
  print_val[result];
end
```

## Features/TODO
- [X] Compile a (somewhat) high-level language down to bytecode that can be run on a virtual machine
    - [X] Impl basic operations, and control flow statements
    - [X] Impl function calls
    - [ ] Impl lexical scoping for blocks
    - [ ] Impl closures
- [ ] Add a bidirectional type-checking algorithm
  - [X] Implement non-polymorphic type checking
  - [ ] Implement polymorphic type checking
- [ ] Add algebraic data types
  - [X] Impl basic variant types
  - [X] Impl tuple types
  - [X] Impl record types (currently called Mod types)
    - [ ] Call them records instead
  - [X] Allow recursive types
  - [ ] Impl `match` expressions
    - [X] Support basic pattern matching
    - [ ] Allow irrefutable patterns in `match` expressions
    - [ ] Impl literal patterns
    - [ ] Impl pattern guards
  - [X] Impl `let-else` expressions
