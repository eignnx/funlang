:- module(tycheck, [
    ast_tast//2,
    typecheck/2,
    op(10, xfy, ::)
]).

:- use_module(lex, [op(12, xfy, @)]).
:- use_module(utils, [
    dcg_maplist1//2,
    dcg_maplist//3,
    state//1,
    op(950, xfx, before_after), before_after//2
]).


ty_err(Msg, Ctx) -->
    { throw(error(ty_err(Msg), Ctx)) }.

define(X :: Ty) --> Tcx0 before_after [X :: Ty | Tcx0].

:- discontiguous ast_tast//2.
% :- det(ast_tast//2).

ast_tast(lit(int(N))@_, ::{tm: lit(int(N)), ty: int}) --> [].
ast_tast(lit(nat(N))@_, ::{tm: lit(nat(N)), ty: nat}) --> [].
ast_tast(lit(bool(B))@_, ::{tm: lit(bool(B)), ty: bool}) --> [].
ast_tast(lit(text(T))@_, ::{tm: lit(text(T)), ty: text}) --> [].

ast_tast(lit(variant(Head, Args0))@_, ::{tm: lit(variant(Head, Args)), ty: variant(Head, Tys)}) -->
    utils:dcg_maplist(ast_tast, Args0, Args),
    { maplist([A, A.ty]>>true, Args, Tys) }.

ast_tast(lit(tuple(Es0))@_, ::{tm: lit(tuple(Es)), ty: tuple(Tys)}) -->
    dcg_maplist(ast_tast, Es0, Es),
    { maplist([E, E.ty]>>true, Es, Tys) }.

ast_tast(lit(list(Xs0))@_, ::{tm: lit(list(Xs)), ty: list(ElTy)}) -->
    lit_list_ast_tast(Xs0, Xs, ElTy).

lit_list_ast_tast([], [], _NewTyVar) --> [].
lit_list_ast_tast([X0@Ln | Xs0], [X | Xs], X.ty) -->
    ast_tast(X0@Ln, X),
    lit_list_ast_tast(Xs0, Xs, XsEleTy),
    ({ X.ty = XsEleTy } -> []
        ; ty_err(list_ele_ty, (X0@Ln)\=Xs)
    ).

ast_tast(binop(Op, A0, B0)@Ln, ::{tm: binop(Op, A, B), ty: Ty}) -->
    ast_tast(A0, A),
    ast_tast(B0, B),
    ({ binop_sig(Op, A.ty->B.ty->Ty) } -> []
        ; ty_err(binop, Op@Ln)
    ).

ast_tast(unop(Op, E0)@Ln, ::{tm: unop(Op, E), ty: Ty}) -->
    ast_tast(E0, E),
    ({ unop_sig(Op, E.ty->Ty) } -> []
        ; ty_err(unop, Op@Ln)
    ).

ast_tast(var(X), ::{tm: var(X), ty: Ty}) --> var_ty(X, Ty).

var_ty(X, Ty) --> state(St), { memberchk(X :: Ty, St) }.

defining(Defs, Body) -->
    state(Saved),
    dcg_maplist1(define, Defs),
    Body,
    _ before_after Saved.

ast_tast(let(X, Expr0)@_, ::{tm: let(X, Expr), ty: void}) -->
    ast_tast(Expr0, Expr),
    define(X :: Expr.ty).

ast_tast(seq(A0, B0)@_, ::{tm: seq(A, B), ty: B.ty}) -->
    state(St0),
    ast_tast(A0, A),
    ast_tast(B0, B),
    _ before_after St0.

ast_tast(if(Cond0, Yes0, No0)@Ln, ::{tm: if(Cond, Yes, No), ty: Ty}) -->
    ast_tast(Cond0, Cond),
    ast_tast(Yes0, Yes),
    ast_tast(No0, No),
    ( { Cond.ty = bool, Yes.ty = Ty, No.ty = Ty } -> []
        ; ty_err_if(Ln, Cond.ty, Yes.ty, No.ty)
    ).

ty_err_if(_Ln, bool, Ty, Ty) --> !.
ty_err_if(Ln, bool, YTy, NTy) --> ty_err(if(yes_no_mismatch), (YTy\=NTy)@Ln), !.
ty_err_if(Ln, NotBool, _, _) --> ty_err(if(cond_not_bool), (expected(bool)\=actual(NotBool))@Ln).

ast_tast(intr(Intr, Arg0)@Ln, ::{tm: intr(Intr, Arg), ty: RetTy}) -->
    ast_tast(Arg0, Arg),
    { intr_sig(Intr, ArgTy->RetTy) },
    ( { Arg.ty = ArgTy } -> []
        ; ty_err(bad_arg_ty(Intr), (Arg.ty\=ArgTy)@Ln)
    ).

ast_tast(lam(Params, Body0)@_, ::{tm: lam(Params, Body), ty: (fn(ParamTys) -> Body.ty)}) -->
    { maplist([_Param, _FreshTyVar]>>true, Params, ParamTys) },
    { maplist([Param, ParamTy, Param::ParamTy]>>true, Params, ParamTys, ParamsAndTys) },
    defining(ParamsAndTys, (
        tycheck:ast_tast(Body0, Body)
    )).

ast_tast(call(Fn0, Args0)@Ln, ::{tm: call(Fn, Args), ty: RetTy}) -->
    ast_tast(Fn0, Fn),
    utils:dcg_maplist(tycheck:ast_tast, Args0, Args),
    { maplist([Arg, Arg.ty]>>true, Args, ArgTys) },
    { Fn.ty = (fn(ParamTys) -> RetTy) },
    ( { maplist(=, ArgTys, ParamTys) } -> []
        ; ty_err(wrong_arg_ty, (expected(ParamTys)\=actual(ArgTys))@Ln)
    ).

ast_tast(
    def{name:Name, params:Params, ret_ty:RetTy, body:Body0}@Ln,
    ::{tm: def{name:Name, params:Defs, ret_ty:RetTy, body:Body}, ty: _}
) -->
    { maplist([param(X, Ty)@_, (X :: Ty)]>>true, Params, Defs) },
    defining(Defs, (
        tycheck:ast_tast(Body0, Body)
    )),
    ({ Body.ty = RetTy } -> []
        ; ty_err(wrong_ret_ty, (expected(RetTy)\=actual(Body.ty))@Ln)
    ).


typecheck(Ast, Tast, Tcx0) :-
    phrase(ast_tast(Ast, Tast), [_Tcx], [Tcx0]).

typecheck(Ast, Tast) :-
    inital_tcx(Tcx0),
    typecheck(Ast, Tast, Tcx0).

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

intr_sig(dbg_int, int->void).
intr_sig(dbg_nat, nat->void).
intr_sig(dbg_bool, bool->void).
intr_sig(dbg_text, text->void).
intr_sig(puts, text->void).
intr_sig(putc, char->void).
