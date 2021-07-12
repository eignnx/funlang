def main[] do
  let greeting = greet-digger["Digger"];
  intr.print[greeting];
end

def greet-digger[name: Text] -> Text do
  if name == "Digger" then
    "You're a good doggy!"
  else
    "Who are you?"
  end
end