# Funlang

It's a fun language-side-project (not that I have a real project to work on tho).

## What it Looks Like
```ruby
# fib[n] = fib[n-1] + fib[n-2]

def main[] do
  n = 100;
  i = 1;
  j = 0;
  intr.here[];
  while n > 0 do
    nxt = i + j;
    j = i;
    i = nxt;
    intr.print[i];
    n = n - 1;
  end
  intr.here[];
  intr.print[i];
end
```

## Features/TODO
- [X] Compile a (somewhat) high-level language down to bytecode that can be run on a virtual machine
    - [X] Implement basic operations, and control flow statements
    - [ ] Implement function calls
    - [ ] Implement closures
- [ ] Add a bidirectional type-checking algorithm
- [ ] Add algebraic data types
