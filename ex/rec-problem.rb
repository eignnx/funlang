def fact[n: Int] =
  if n < 2 then
    n
  else
    do
      if fact[1] then nop else nop end
      #  ^^^^^^^ This is a problem!
      n * fact[n - 1]
    end
  end

def main[] do
  intr.dbg_int[fact[10]];
end

# The problem is that function definitions without an explicit return type are
# temporarily given return type `Never`. This allows `fact[1]` to typecheck when
# placed in an `if` condition. Of course this is invalid, but the type checker
# doesn't catch it, and when run, this program crashes the VM.