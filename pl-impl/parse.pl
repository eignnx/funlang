:- use_module(library(dcg/high_order)).
:- use_module(lex, [tokens//1]).
:- set_prolog_flag(double_quotes, chars).

parse(Nonterminal, Src) :-
    phrase(tokens(Ts), Src),
    phrase(Nonterminal, Ts).

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
ty(alias(Name, Params)) -->
    [id(Name)],
    optional(
        (
            [sym('[')],
            sequence(ty, [sym(',')], Params),
            [sym(']')]
        ),
        { Params = [] }
    ).

expr(Expr) --> expr_rec(Expr).

% Operator Deference Table ("deference" is the inverse of precedence)
% ========================
lvl_op(6, or). lvl_op(6, xor).
lvl_op(5, and).
lvl_op(4, '=='). lvl_op(4, '!='). lvl_op(4, '>'). lvl_op(4, '<'). lvl_op(4, '>='). lvl_op(4, '<=').
lvl_op(3, '+'). lvl_op(3, '-'). lvl_op(3, '++').
lvl_op(2, '*'). lvl_op(2, '/').
lvl_op(1, '**'). lvl_op(1, '^').

lvl_op(Lvl, Op) --> % To parse an operator of a certain level...
    { lvl_op(Lvl, Op) }, % ...first find an op of the given level...
    ( [sym(Op)] | [kw(Op)] ). % ...then try to parse it.

max_deference_lvl(Lvl) :-
    lvl_op(Lvl, _), !. % Just find the first one listed.

expr_rec(E) --> { max_deference_lvl(Lvl) }, expr_rec(Lvl, E).

expr_rec(0, E) --> !,
    expr_norec(E). % A "level-0" expression is one without left-recursion.
expr_rec(Lvl, E) -->
    expr_rec_below(Lvl, E1), % First parse a term one level lower in the "operator deference" table.
    expr_rec_follow(Lvl, E1->E). % The, passing E1 as an argument to `..._follow`, parse the rest of the term.
expr_rec_follow(Lvl, E1->E) -->
    lvl_op(Lvl, Op), !, % Try to parse the operator itself. Commit if found.
    expr_rec_below(Lvl, E2), % Parse a term one level lower in the "operator deference" table.
    { E1E2 = binop(Op, E1, E2) }, % Construct the actual term.
    expr_rec_follow(Lvl, E1E2->E). % Try to continue parsing at this level. (E1E2 would be used as the next E1).
expr_rec_follow(_Lvl, E->E) --> []. % If you can't parse another term at this level, just yield what you've got.

% Just a helper :)
expr_rec_below(Lvl, E) -->
    { Below is Lvl - 1 }, expr_rec(Below, E).

:- det(expr_norec//1).

% An expression who's grammar rule does NOT contain left recursion.
expr_norec(lit(int(I))) --> [lit(int(I))], !.
expr_norec(lit(nat(I))) --> [lit(nat(I))], !.
expr_norec(lit(bool(B))) --> [lit(bool(B))], !.
expr_norec(if(Cond, Yes, No)) -->
    [kw(if)], !, expr(Cond),
    ( [kw(do)], ! | [kw(then)] ), expr(Yes),
    [kw(else)], expr(No),
    [kw(end)].
expr_norec(let(X, Expr)) --> [kw(let)], !, [id(X), sym(=)], expr(Expr).
expr_norec(var(X)) --> [id(X)], !.
expr_norec(block(Expr)) --> [kw(do)], !, expr(Expr), [kw(end)].
expr_norec(match(Scrut, Arms)) -->
    [kw(match)], !,
    expr(Scrut),
    sequence(match_arm, Arms),
    [kw(end)].
expr_norec(Expr) --> [sym('(')], !, expr(Expr), [sym(')')].
expr_norec(unop('-', Expr)) --> [sym('-')], !, expr(Expr).
expr_norec(unop('!', Expr)) --> [sym('!')], !, expr(Expr).
expr_norec(unop('~', Expr)) --> [sym('~')], !, expr(Expr).


match_arm(case(Pat, Expr)) -->
    [sym('|')],
    pattern(Pat),
    [sym(=>)],
    expr(Expr).

pattern(refut(variant(Name, Params))) -->
    [sym('{'), sym(:)], !, [id(Name)],
    sequence(pattern, [sym(',')], Params),
    [sym('}')].
pattern(refut(lit(Literal))) --> [lit(Literal)], !.
pattern(var(Name)) --> [id(Name)].

/*
phrase(expr(E),
    [kw(if), lit(bool(true)), kw(then), lit(int(123)), sym(+), lit(int(456)), kw(else), lit(int(999)), kw(end)]
).
*/