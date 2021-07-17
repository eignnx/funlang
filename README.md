# Funlang

It's a fun language-side-project (not that I have a real project to work on tho).

## What it Looks Like
```ruby
# fib[n] = fib[n-1] + fib[n-2]

def fib[n: Int] -> Int do
  if n < 2 then
    n
  else
    fib[n - 2] + fib[n - 1]
  end
end

def main[] do
  let n = 20;
  intr.print["Working..."];
  intr.print[fib[n]];
  intr.print["Done!"];
end
```

## Features/TODO
- [X] Compile a (somewhat) high-level language down to bytecode that can be run on a virtual machine
    - [X] Implement basic operations, and control flow statements
    - [X] Implement function calls
    - [X] Implement lexical scoping for blocks
    - [ ] Implement closures
- [ ] Add a bidirectional type-checking algorithm
  - [X] Implement non-polymorphic type checking
  - [ ] Implement polymorphic type checking
- [ ] Add algebraic data types
