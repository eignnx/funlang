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
  end
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
  end
end

def main[] do
  let ctx = { :Empty };
  let expr =
    { :Let "x", { :Num 123 },
      { :Add { :Mul { :Num 2 }, { :Num 3 } }, { :Var "x" } } };
  let {ctx, result} = eval[ctx, expr];
  intr.dbg_int[result];
end
