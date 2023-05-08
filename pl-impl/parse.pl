% item(fn_def(Name, Params, Body)) -->
%     [kw(def)], !, [id(Name)], param_list(Params), [kw(do)], expr(Body), [kw(end)].

% param(param(Name, Type)) --> [id(Name), sym(:)], type(Type).

% param_list([]) --> [sym(oparen)], !, [sym(cparen)].
% param_list([]) --> [sym(oparen)], !, [sym(cparen)].
% param_list([]) --> [sym(oparen)], !, [sym(cparen)].


% data Seq e -- <sequence> -->
%   = Empty --            | lookahead{END}
%   | Result e --            | <expr> lookahead{END}
%   | Semi e (Seq e) --            | <expr> SEMICOLON <sequence>

:- table expr//1.

expr(lit(int(I))) --> [lit(int(I))], !.
expr(lit(bool(B))) --> [lit(bool(B))], !.
expr(add(A, B)) --> expr(A), [sym(+)], !, expr(B).
expr(sub(A, B)) --> expr(A), [sym(-)], !, expr(B).
expr(if(Cond, Yes, No)) -->
    [kw(if)], !, expr(Cond), [kw(then)], expr(Yes), [kw(else)], expr(No), [kw(end)].
expr(let(X, Expr)) --> [kw(let)], !, [id(X), sym(=)], expr(Expr).
expr(seq(A, B)) --> expr(A), [sym(;)], !, expr(B).
expr(var(X)) --> [id(X)], !.
expr(Expr) --> { throw(error(unimplemented(Expr))) }.

/*
phrase(expr(E),
    [kw(if), lit(bool(true)), kw(then), lit(int(123)), sym(+), lit(int(456)), kw(else), lit(int(999)), kw(end)]
).
*/