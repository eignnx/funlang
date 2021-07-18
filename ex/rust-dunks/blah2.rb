def takes-a-never-like-no-prahblem[] -> Never do
  if loop do end then
    nop
  else
    nop
  end; # <- semicolon: A-OK
end

def stop-it-patrick-your-scaring-him[] -> Never do
  if loop do end then
    nop
  else
    nop
  end # <- no semicolon: Also OK!
end

def main[] = nop