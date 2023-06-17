:- module(parse, [item//1, ty//1, expr//1]).

:- use_module(library(dcg/high_order)).
:- use_module(lex, [tokens//1, op(12, xfy, @)]).
:- set_prolog_flag(double_quotes, chars).

%! parse(?Nonterminal, +SourceCode:list(char)).
%
%
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


:- dynamic max_deference_lvl/1.
:- (abolish(max_deference_lvl/1),
    findall(Lvl, lvl_op(Lvl, _), Lvls),
    max_member(MaxLvl, Lvls),
    assertz(max_deference_lvl(MaxLvl))
).


expr(Expr) --> expr_rec(Expr).

expr_rec(E) -->
    { max_deference_lvl(Lvl) },
    expr_rec(Lvl, E).
expr_rec(0, E) --> !, % A "level-0" expression is one without left-recursion.
    expr_norec(E).
expr_rec(Lvl, E) -->
    expr_rec_below(Lvl, E1),     % First parse a term one level lower in the "operator deference" table.
    expr_rec_follow(Lvl, E1->E). % Then, passing E1 as an argument to `..._follow`, parse the rest of the term.

expr_rec_follow(Lvl, E1->E) -->
    lvl_op(Lvl, Op@Ln), !,           % Try to parse the operator itself. Commit if found.
    expr_rec_below(Lvl, E2),         % Parse a term one level lower in the "operator deference" table.
    { E1E2 = binop(Op, E1, E2)@Ln }, % Construct the actual term.
    expr_rec_follow(Lvl, E1E2->E).   % Try to continue parsing at this level. (E1E2 would be used as the next E1).
expr_rec_follow(_Lvl, E->E) --> !.   % If you can't parse another term at this level, just yield what you've got.
expr_rec_follow(Lvl, E1->E) -->
    { throw(error(nonexhastive(expr_rec_follow(Lvl, E1->E)))) }.

% Just a helper :)
expr_rec_below(Lvl, E) -->
    { Below is Lvl - 1 }, expr_rec(Below, E).

comma_sep(DcgBody, Items) -->
    sequence(DcgBody, [sym(',')@_], Items).


% An expression who's grammar rule does NOT contain left recursion.
expr_norec(lit(int(I))@Ln) --> [lit(int(I))@Ln], !.
expr_norec(lit(nat(I))@Ln) --> [lit(nat(I))@Ln], !.
expr_norec(lit(bool(B))@Ln) --> [lit(bool(B))@Ln], !.
expr_norec(lit(text(T))@Ln) --> [lit(text(T))@Ln], !.
expr_norec(lit(variant(Head, Es))@Ln) -->
    [sym('{')@Ln, sym(:)@_], !,
    [id(Head)@_],
    comma_sep(expr, Es),
    [sym('}')@_].
expr_norec(lit(tuple(Es))@Ln) -->
    [sym('{')@Ln], !,
    comma_sep(expr, Es),
    [sym('}')@_].
expr_norec(lit(list(Es))@Ln) -->
    [sym('[')@Ln], !,
    comma_sep(expr, Es),
    [sym(']')@_].
expr_norec(intr(Intr, E)@Ln) -->
    [kw(intr)@Ln, sym('.')@_],
    { intrinsic_name(Intr) }, [id(Intr)@_], !,
    [sym('[')@_],
    expr(E),
    [sym(']')@_].
expr_norec(if(Cond, Yes, No)@Ln) -->
    [kw(if)@Ln], !,
    expr(Cond),
    ( [kw(do)@_] | [kw(then)@_] ), !, expr(Yes),
    [kw(else)@_], expr(No),
    [kw(end)@_].
expr_norec(let(X, Expr)@Ln) --> [kw(let)@Ln], !, [id(X)@_, sym(=)@_], expr(Expr), [sym(';')@_].
expr_norec(block(Expr)@Ln) --> [kw(do)@Ln], !, expr(Expr), [kw(end)@_].
expr_norec(lam(Params, Body)@Ln) -->
    [sym('|')@Ln], !, comma_sep(id, Params), [sym('|')@_],
    expr(Body).
expr_norec(match(Scrut, Arms)@Ln) -->
    [kw(match)@Ln], !,
    expr(Scrut),
    sequence(match_arm, Arms),
    [kw(end)@_].
expr_norec(unop('-', Expr)@Ln) --> [sym('-')@Ln], !, expr(Expr).
expr_norec(unop(not, Expr)@Ln) --> [kw(not)@Ln], !, expr(Expr).
expr_norec(unop('~', Expr)@Ln) --> [sym('~')@Ln], !, expr(Expr).
expr_norec(call_direct(Fn, Args)@Ln) -->
    [id(Fn)@Ln, sym('[')@_], !,
    sequence(expr, [sym(',')@_], Args),
    [sym(']')@_].
expr_norec(var(X)@Ln) --> [id(X)@Ln], !.
expr_norec(call_indirect(Fn, Args)@Ln) -->
    [sym('(')@Ln], expr(Fn), [sym(')')@_],
    [sym('[')@_], !,
    sequence(expr, [sym(',')@_], Args),
    [sym(']')].
expr_norec(Expr) --> [sym('(')@_], !, expr(Expr), [sym(')')@_].
expr_norec(OTHER) -->
    { throw(error(nonexhastive(expr_norec(OTHER)))) }.


match_arm(case(Pat, Expr)@Ln) -->
    [sym('|')@Ln], pattern(Pat), [sym(=>)@_], expr(Expr).

pattern(refut(variant(Name, Params))@Ln) -->
    [sym('{')@Ln, sym(:)@_], !, [id(Name)@_],
    sequence(pattern, [sym(',')@_], Params),
    [sym('}')@_].
pattern(irrefut(tuple(Ps))@Ln) -->
    [sym('{')@Ln], !,
    sequence(pattern, [sym(',')@_], Ps),
    [sym('}')@_].
pattern(refut(lit(Literal))@Ln) --> [lit(Literal)@Ln], !.
pattern(irrefut(var(Name))@Ln) --> [id(Name)@Ln].

intrinsic_name(dbg_nat).
intrinsic_name(dbg_int).
intrinsic_name(dbg_bool).
intrinsic_name(dbg_text).
intrinsic_name(puts).
intrinsic_name(putc).

id(Ident) --> [id(Ident)@_].