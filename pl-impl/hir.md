# `hir.md`
## High-level Structure
HIR is made up of a *pair-list* representing `item`s.

An `item` is a top-level definition. It could be:
- a function definition
- a static constant definition

All `item`s have a unique symbol associated with them, and a body/payload.

## Example

```ruby
# funlang source:

def main[] do
    let local_hello = helper[];
end

def helper[] -> Text do
    if true do nop else nop end
    "hello!"
end
```

```lua
-- Generated HIR facts:

item_name_value(
    main,
    hir([
        call(helper, 0),
        store(local(local_hello) :: text),
        ret,
    ])
).

item_name_value(
    helper,
    hir([
        const(bool(true)),
        jmp_if_false('$if_else_0'),
            nop,
        jmp('$if_end_0'),
        label('$if_else_0'),
            nop,
        label('$if_end_0'),
        const(ptr, static_text('$static_text_0')),
        const(nat(6)), -- the string length
        ret,
    ])
).

item_name_value(
    '$static_text_0',
    static_text(
        "hello!"
    )
).
```