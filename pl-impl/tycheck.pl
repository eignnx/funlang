:- op(10, xfy, ::).

:- discontiguous ast_tast//2.
% :- det(ast_tast//2).

ast_tast(lit(int(N)), lit(int(N)) :: int) --> [].
ast_tast(lit(nat(N)), lit(nat(N)) :: nat) --> [].

ast_tast(lit(bool(B)), lit(bool(B)) :: bool) --> [].

ast_tast(lit(list(Xs0)), lit(list(Xs)) :: list(ElTy)) -->
    lit_list_ast_tast(Xs0, Xs, ElTy).

lit_list_ast_tast([], [], _) --> [].
lit_list_ast_tast([X0 | Xs0], [X :: XTy | Xs], XTy) -->
    ast_tast(X0, X :: XTy),
    lit_list_ast_tast(Xs0, Xs, XTy).

ast_tast(binop(Op, A0, B0), binop(Op, A :: T1, B :: T2) :: T3) -->
    ast_tast(A0, A :: T1),
    ast_tast(B0, B :: T2),
    { binop_sig(Op, T1->T2->T3) }.

ast_tast(unop(Op, E), unop(Op, A :: T1) :: T2) -->
    ast_tast(A0, E :: T1),
    { unop_sig(Op, T1->T2) }.

ast_tast(var(X), var(X) :: Ty) --> var_ty(X, Ty).

var_ty(X, Ty) --> state(St), { memberchk(X :: Ty, St) }.

ast_tast(let(X, Expr0), let(X, Expr :: Ty) :: void) -->
    ast_tast(Expr0, Expr :: Ty),
    define(X :: Ty).

define(X :: Ty) --> state(Tcx0, [X :: Ty | Tcx0]).

ast_tast(seq(A0, B0), seq(A :: ATy, B :: BTy) :: BTy) -->
    state(St0),
    ast_tast(A0, A :: ATy),
    ast_tast(B0, B :: BTy),
    state(_, St0).

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

ast_tast(if(Cond0, Yes0, No0), if(Cond::bool, Yes::Ty, No::Ty) :: Ty) -->
    ast_tast(Cond0, Cond :: bool),
    ast_tast(Yes0, Yes :: Ty),
    ast_tast(No0, No :: Ty).

ast_tast(intr(dbg_int, Arg0), intr(dbg_int, Arg :: int) :: void) -->
    ast_tast(Arg0, Arg :: int).

ast_tast(intr(dbg_bool, Arg0), intr(dbg_bool, Arg :: bool) :: void) -->
    ast_tast(Arg0, Arg :: bool).

ast_tast(lam(Param, Body0), lam(Param, Body) :: (ParamTy -> RetTy)) -->
    state(St0),
    define(Param :: ParamTy),
    ast_tast(Body0, Body),
    { Body = _ :: RetTy },
    state(_, St0).

ast_tast(call(Fn0, Arg0), call(Fn, Arg) :: RetTy) -->
    ast_tast(Fn0, Fn :: (ParamTy -> RetTy)),
    ast_tast(Arg0, Arg),
    { Arg = _ :: ArgTy },
    { ArgTy = ParamTy }.


tycheck(Ast, Tast :: Ty, Tcx0) :-
    phrase(ast_tast(Ast, Tast :: Ty), [_Tcx], [Tcx0]).

tycheck(Ast, Tast :: Ty) :-
    inital_tcx(Tcx0),
    tycheck(Ast, Tast :: Ty, Tcx0).

inital_tcx([]).


binop_sig(Op, bool->bool->bool) :- memberchk(Op, ['and', 'or', 'xor']).
binop_sig(Op, Ty->Ty->bool) :-
    memberchk(Op, ['>', '<', '>=', '<=']),
    memberchk(Ty, [nat, int]).
binop_sig(Op, Ty->Ty->bool) :- memberchk(Op, ['==', '!=']).
binop_sig(Op, Ty->Ty->Ty) :-
    memberchk(Op, ['+', '-', '*', '/']),
    memberchk(Ty, [nat, int]).
binop_sig('^', nat->nat->nat).
binop_sig('++', text->text->text).
binop_sig('**', Ty->nat->Ty) :- memberchk(Ty, [text, list(_), array(_)]).

unop_sig('-', int->int).
unop_sig('!', bool->bool).
unop_sig('~', nat->nat). % TODO: Allow more types here?
