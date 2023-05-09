:- use_module(library(dcg/high_order)).


item(def(Name, Body, RetTy)) -->
    [kw(def)], !, [id(Name), sym('[')],
    [],
    [sym(']'), sym(->), ty(RetTy), kw(do)],
    expr(Body),
    [kw(end)].

item(type(Name, Variants)) -->
    [kw(type)], optional([kw(rec)], []), [id(Name)],
    sequence(variant_arm, Variants),
    [kw(end)].

variant_arm(ctor(Name, Tys)) -->
    [sym('|'), sym(:), id(Name)], !, sequence(ty, [sym(',')], Tys).
variant_arm(supertype(Name)) --> [sym('|'), id(Name)].

ty(prim(void)) --> [id('Void')].
ty(prim(bool)) --> [id('Bool')].
ty(prim(int)) --> [id('Int')].
ty(prim(nat)) --> [id('Nat')].
ty(alias(Name)) --> [id(Name)].

:- table expr//1.

expr(lit(int(I))) --> [lit(int(I))], !.
expr(lit(bool(B))) --> [lit(bool(B))], !.
expr(add(A, B)) --> expr(A), [sym(+)], !, expr(B).
expr(sub(A, B)) --> expr(A), [sym(-)], !, expr(B).
expr(if(Cond, Yes, No)) -->
    [kw(if)], !, expr(Cond),
    [kw(then)], expr(Yes),
    [kw(else)], expr(No),
    [kw(end)].
expr(let(X, Expr)) --> [kw(let)], !, [id(X), sym(=)], expr(Expr).
expr(seq(A, B)) --> expr(A), [sym(;)], !, expr(B).
expr(var(X)) --> [id(X)], !.
expr(block(Expr)) --> [kw(do)], !, expr(Expr), [kw(end)].
expr(Expr) --> { throw(error(unimplemented(Expr))) }.

/*
phrase(expr(E),
    [kw(if), lit(bool(true)), kw(then), lit(int(123)), sym(+), lit(int(456)), kw(else), lit(int(999)), kw(end)]
).
*/