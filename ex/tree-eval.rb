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
        let { ctx, { :Num x } } = eval[ctx, x] else
          type_error["The `+` operator requires numbers!"];
        end
        let { ctx, { :Num y } } = eval[ctx, y] else
          type_error["The `+` operator requires numbers!"];
        end
        { ctx, { :Num x + y } }
    | { :Mul x, y } =>
        let { ctx, { :Num x } } = eval[ctx, x] else
          type_error["The `+` operator requires numbers!"];
        end
        let { ctx, { :Num y } } = eval[ctx, y] else
          type_error["The `+` operator requires numbers!"];
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
      { :If { :Bool true},
        { :Add { :Mul { :Num 2 }, { :Num 3 } }, { :Var "x" } },
        { :Mul { :Num 5 } } } };
  let { ctx, result } = eval[ctx, expr];
  print_val[result];
end
