def never-if[] -> Int do
  if true then
    intr.exit[]
  else
    1
  end
end

def main[] = nop