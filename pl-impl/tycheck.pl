:- module(tycheck, [
    ast_tast//2,
    op(10, xfy, ::)
]).

:- use_module(lex, op(120, xfy, @)).


ty_err(Msg, Ctx) -->
    { throw(error(ty_err(Msg), Ctx)) }.

:- discontiguous ast_tast//2.
% :- det(ast_tast//2).

ast_tast(lit(int(N))@_, lit(int(N)) :: int) --> [].
ast_tast(lit(nat(N))@_, lit(nat(N)) :: nat) --> [].

ast_tast(lit(bool(B))@_, lit(bool(B)) :: bool) --> [].

ast_tast(lit(list(Xs0))@_, lit(list(Xs)) :: list(ElTy)) -->
    lit_list_ast_tast(Xs0, Xs, ElTy).

lit_list_ast_tast([], [], _NewTyVar)) --> [].
lit_list_ast_tast([X0@Ln | Xs0], [X :: XTy | Xs], XTy) -->
    ast_tast(X0@Ln, X :: XTy),
    lit_list_ast_tast(Xs0, Xs, XsEleTy),
    { XTy = XsEleTy } -> []
        ; ty_err(list_ele_ty, X0@Ln\=Xs).

ast_tast(binop(Op, A0, B0)@Ln, binop(Op, A :: T1, B :: T2) :: T3) -->
    ast_tast(A0, A :: T1),
    ast_tast(B0, B :: T2),
    !,
    { binop_sig(Op, T1->T2->T3) } -> []
        ; ty_err(binop, Op@Ln).

ast_tast(unop(Op, E0)@Ln, unop(Op, E :: T1) :: T2) -->
    ast_tast(E0, E :: T1),
    !,
    { unop_sig(Op, T1->T2) } -> []
        ; ty_err(unop, Op@Ln).

ast_tast(var(X), var(X) :: Ty) --> var_ty(X, Ty).

var_ty(X, Ty) --> state(St), { memberchk(X :: Ty, St) }.

ast_tast(let(X, Expr0)@_, let(X, Expr :: Ty) :: void) -->
    ast_tast(Expr0, Expr :: Ty),
    define(X :: Ty).

define(X :: Ty) --> state(Tcx0->[X :: Ty | Tcx0]).
defining(Defs, Body) -->
    state(Saved),
    sequence([Name :: Ty]>>define(Name :: Ty), Defs),
    Body,
    state(_->Saved).

ast_tast(seq(A0, B0)@_, seq(A :: ATy, B :: BTy) :: BTy) -->
    state(St0),
    ast_tast(A0, A :: ATy),
    ast_tast(B0, B :: BTy),
    state(_->St0).

state(S), [S] --> [S].
state(S0->S), [S] --> [S0].

ast_tast(if(Cond0, Yes0, No0)@Ln, if(Cond::bool, Yes::Ty, No::Ty) :: Ty) -->
    ast_tast(Cond0, Cond :: CTy),
    ({ CTy = bool } -> []
        ; ty_err(if_cond_not_bool, CTy@Ln))
    ast_tast(Yes0, Yes :: YTy),
    ast_tast(No0, No :: NTy),
    ({ YTy = NTy } -> []
        ; ty_err(if_yes_no_mismatch, YTy\=NTy)).

ast_tast(intr(dbg_int, Arg0)@_, intr(dbg_int, Arg :: int) :: void) -->
    ast_tast(Arg0, Arg :: int).

ast_tast(intr(dbg_bool, Arg0)@_, intr(dbg_bool, Arg :: bool) :: void) -->
    ast_tast(Arg0, Arg :: bool).

ast_tast(lam(Param, Body0)@_, lam(Param, Body) :: (ParamTy -> RetTy)) -->
    defining([Param :: ParamTy], (
        ast_tast(Body0, Body),
        { Body = _ :: RetTy }
    )).

ast_tast(call(Fn0, Arg0)@Ln, call(Fn, Arg) :: RetTy) -->
    ast_tast(Fn0, Fn),
    ast_tast(Arg0, Arg),
    { Fn = _ :: (ParamTy -> RetTy) },
    { Arg = _ :: ArgTy },
    ( { ArgTy = ParamTy } -> []
      ; ty_err(wrong_arg_ty, (expected(ParamTy)\=actual(ArgTy))@Ln)
    ).

ast_tast(
    def{name:Name, params:Params, ret_ty:RetTy, body:Body0}@Ln,
    def{name:Name, params:Params, ret_ty:RetTy, body:Body} :: _
) -->
    { maplist([param(X, Ty)@_, (X :: Ty)]>>true, Params, Defs) },
    defining(Defs, (
        ast_tast(Body0, Body)
    )),
    { _ :: BodyTy = Body },
    ({ BodyTy = RetTy } -> []
        ; ty_err(wrong_ret_ty, expected(RetTy)\=actual(BodyTy))).


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
