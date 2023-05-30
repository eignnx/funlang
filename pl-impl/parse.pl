:- module(parse, [item//1, ty//1, expr//1]).

:- use_module(library(dcg/high_order)).
:- use_module(lex, [tokens//1, op(12, xfy, @)]).
:- set_prolog_flag(double_quotes, chars).

parse(Nonterminal, Src) :-
    phrase(tokens(Ts), Src),
    phrase(Nonterminal, Ts).

??(Msg, Line) --> { true } ; { throw(error(Msg, Line)) }.

:- discontiguous item//1.

item(def{name:Name, params:Params, ret_ty:RetTy, body:Body}@Ln) -->
    [kw(def)@Ln], %??('expected valid function definition to follow `def`', Ln),
    [id(Name)@_, sym('[')@_],
    sequence(param, [sym(',')@_], Params),
    [sym(']')@_],
    optional(([sym(->)@_], ty(RetTy)), { RetTy = void }),
    [kw(do)@_],
    expr(Body),
    [kw(end)@_],
    !.

param(param(Id, Ty)@Ln) -->
    [id(Id)@Ln], ??('expected `: TYPE` to follow paramteter name', Ln),
    [sym(':')@_], ty(Ty), !.

item(type(Name, Variants)) -->
    [kw(type)@Ln], ??('expected valid type definition to follow `type`', Ln),
    optional([kw(rec)@_], []), [id(Name)@_],
    sequence(variant_arm, Variants),
    [kw(end)@_],
    !.

variant_arm(ctor(Name, Tys)@Ln) -->
    [sym('|')@_, sym(:)@_, id(Name)@Ln], ??('expected comma separated list of types', Ln),
    sequence(ty, [sym(',')@_], Tys), !.
variant_arm(supertype(Name)@Ln) --> [sym('|')@_, id(Name)@Ln].

ty(void) --> [id('Void')@_], !.
ty(bool) --> [id('Bool')@_], !.
ty(int) --> [id('Int')@_], !.
ty(nat) --> [id('Nat')@_], !.
ty(fn(Params) -> RetTy) -->
    [kw(fn)@_], !, [sym('[')@_],
    sequence(ty, [sym(',')@_], Params),
    [sym(']')@_], optional(([sym('->')@_], ty(RetTy)), { RetTy = void }).
ty(alias(Name, Params)) -->
    [id(Name)@_],
    optional(
        (
            [sym('[')@_],
            sequence(ty, [sym(',')@_], Params),
            [sym(']')@_]
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

lvl_op(Lvl, Op@Ln) --> % To parse an operator of a certain level...
    { lvl_op(Lvl, Op) }, % ...first find an op of the given level...
    ( [sym(Op)@Ln] | [kw(Op)@Ln] ). % ...then try to parse it.

max_deference_lvl(Lvl) :-
    lvl_op(Lvl, _), !. % Just find the first one listed.

expr_rec(E) --> { max_deference_lvl(Lvl) }, expr_rec(Lvl, E).

expr_rec(0, E) --> !,
    expr_norec(E). % A "level-0" expression is one without left-recursion.
expr_rec(Lvl, E) -->
    expr_rec_below(Lvl, E1), % First parse a term one level lower in the "operator deference" table.
    expr_rec_follow(Lvl, E1->E). % The, passing E1 as an argument to `..._follow`, parse the rest of the term.
expr_rec_follow(Lvl, E1->E) -->
    lvl_op(Lvl, Op@Ln), !, % Try to parse the operator itself. Commit if found.
    expr_rec_below(Lvl, E2), % Parse a term one level lower in the "operator deference" table.
    { E1E2 = binop(Op, E1, E2)@Ln }, % Construct the actual term.
    expr_rec_follow(Lvl, E1E2->E). % Try to continue parsing at this level. (E1E2 would be used as the next E1).
expr_rec_follow(_Lvl, E->E) --> []. % If you can't parse another term at this level, just yield what you've got.

% Just a helper :)
expr_rec_below(Lvl, E) -->
    { Below is Lvl - 1 }, expr_rec(Below, E).

:- det(expr_norec//1).

% An expression who's grammar rule does NOT contain left recursion.
expr_norec(lit(int(I))@Ln) --> [lit(int(I))@Ln], !.
expr_norec(lit(nat(I))@Ln) --> [lit(nat(I))@Ln], !.
expr_norec(lit(bool(B))@Ln) --> [lit(bool(B))@Ln], !.
expr_norec(if(Cond, Yes, No)@Ln) -->
    [kw(if)@Ln], !, expr(Cond),
    ( [kw(do)@_], ! | [kw(then)@_] ), expr(Yes),
    [kw(else)@_], expr(No),
    [kw(end)@_].
expr_norec(let(X, Expr)@Ln) --> [kw(let)@Ln], !, [id(X)@_, sym(=)@_], expr(Expr), [sym(';')@_].
expr_norec(var(X)@Ln) --> [id(X)@Ln], !.
expr_norec(block(Expr)@Ln) --> [kw(do)@Ln], !, expr(Expr), [kw(end)@_].
expr_norec(match(Scrut, Arms)@Ln) -->
    [kw(match)@Ln], !,
    expr(Scrut),
    sequence(match_arm, Arms),
    [kw(end)@_].
expr_norec(Expr) --> [sym('(')@_], !, expr(Expr), [sym(')')@_].
expr_norec(unop('-', Expr)@Ln) --> [sym('-')@Ln], !, expr(Expr).
expr_norec(unop('!', Expr)@Ln) --> [sym('!')@Ln], !, expr(Expr).
expr_norec(unop('~', Expr)@Ln) --> [sym('~')@Ln], !, expr(Expr).


match_arm(case(Pat, Expr)@Ln) -->
    [sym('|')@Ln],
    pattern(Pat),
    [sym(=>)@_],
    expr(Expr).

pattern(refut(variant(Name, Params))@Ln) -->
    [sym('{')@Ln, sym(:)@_], !, [id(Name)@_],
    sequence(pattern, [sym(',')@_], Params),
    [sym('}')@_].
pattern(refut(lit(Literal))@Ln) --> [lit(Literal)@Ln], !.
pattern(irrefut(var(Name))@Ln) --> [id(Name)@Ln].

/*
phrase(expr(E),
    [kw(if), lit(bool(true)), kw(then), lit(int(123)), sym(+), lit(int(456)), kw(else), lit(int(999)), kw(end)]
).
*/