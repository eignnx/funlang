def takes_a_never_like_no_prahblem[] -> Never do
  if loop do end then
    nop
  else
    nop
  end; # <- semicolon: A-OK
end

def stop_it_patrick_your_scaring_him[] -> Never do
  if loop do end then
    nop
  else
    nop
  end # <- no semicolon: Also OK!
end

def main[] = nop