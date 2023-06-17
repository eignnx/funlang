:- module(tast_to_hir, [
    gen_hir_from_items/1,
    item_name_value/2
]).

:- use_module(ty, [type_size/2]).
:- use_module(hir, [syscall_number/2]).
:- use_module(utils, [dcg_maplist//2, dcg_maplist//3]).

:- op(10, xfy, ::).

% Facts for `item_name_value` will be asserted during HIR generation.
:- dynamic item_name_value/2.

% :- det(hir//1).
%! hir(+TmTyDict)//
%
% @see ./hir.md for high-level details about this translation.
hir(::{tm:Tm, ty:Ty}) --> hir_(Tm::Ty).

% :- det(hir_//1).
:- discontiguous hir_//1.

hir_(lit(int(I)) :: _) --> [const(qword, int(I))].
hir_(lit(nat(I)) :: _) --> [const(qword, nat(I))].
hir_(lit(bool(B)) :: _) --> [const(byte, bool(B))].
hir_(lit(text(Txt)) :: _) -->
    { gensym('$static_text_', Lbl) },
    [const(ptr, static_text(Lbl))],
    { length(Txt, Len) },
    [const(qword, nat(Len))],
    { assertz( item_name_value(Lbl, static_text(Txt)) ) }.

hir_(lit(tuple(Es)) :: TupleTy) -->
    [alloc(sizeof(TupleTy))],
    { length(Es, N), numlist(0, N, LogicalIndices) },
    dcg_maplist(hir_tuple_field, Es, LogicalIndices).

hir_tuple_field(Expr, LogicalIndex) -->
    hir(Expr), % Compute the value to be written: [Val, Ptr | Rest]
    [mem_write_direct(LogicalIndex)]. % Write the bytes on the stack: [Ptr | Rest]

hir_(lit(variant(_Head, Args)) :: VariantTy) -->
    { maplist([Arg, Arg.ty]>>true, Args, ArgTys) },

    % { maplist(\($2 = ($1).ty), Args, ArgTys) },
    % { maplist(+(+2 = (+1).ty), Args, ArgTys) },
    % { maplist(*(*2 = (*1).ty), Args, ArgTys) },
    % { maplist(?(?2 = (?1).ty), Args, ArgTys) },
    % { maplist(~(~2 = (~1).ty), Args, ArgTys) },
    % { maplist(^(^2 = (^1).ty), Args, ArgTys) },
    % { maplist(&(&2 = (&1).ty), Args, ArgTys) },
    % { maplist(@(@2 = (@1).ty), Args, ArgTys) },
    % { maplist($($2 = ($1).ty), Args, ArgTys) },
    % { maplist((#2 = (#1).ty)&, Args, ArgTys) },

    { TupleTy  = [discr(VariantTy)      | ArgTys] },
    { TupleVal = [discr(VariantTy) :: _ |   Args] },
    hir_(lit(tuple(TupleVal)) :: TupleTy).

hir_(discr(VariantTy) :: _) --> [gen_discr(VariantTy)].

hir_(binop(+,    A, B) :: Ty) --> hir(A), hir(B), [add(Ty)].
hir_(binop(-,    A, B) :: Ty) --> hir(A), hir(B), [sub(Ty)].
hir_(binop(*,    A, B) :: Ty) --> hir(A), hir(B), [mul(Ty)].
hir_(binop(/,    A, B) :: Ty) --> hir(A), hir(B), [div(Ty)].
hir_(binop(and,  A, B) ::  _) --> hir(A), hir(B), [and].
hir_(binop(or,   A, B) ::  _) --> hir(A), hir(B), [or].
hir_(binop(xor,  A, B) ::  _) --> hir(A), hir(B), [over, over, or, rot, rot, and, not, and].
hir_(binop('>',  A, B) :: Ty) --> hir(A), hir(B), [gt(Ty)].
hir_(binop('<',  A, B) :: Ty) --> hir(A), hir(B), [lt(Ty)].
hir_(binop('==', A, B) ::  _) --> hir(A), hir(B), [eq].
hir_(binop('!=', A, B) ::  _) --> hir(A), hir(B), [eq, not].
hir_(binop('>=', A, B) :: Ty) --> hir(A), hir(B), [over, over, gt(Ty), eq, or].
hir_(binop('<=', A, B) :: Ty) --> hir(A), hir(B), [over, over, lt(Ty), eq, or].

hir_(unop(not, E) :: _) --> hir(E), [not].
hir_(unop('-', E) :: _) --> hir(E), [neg(E.ty)].
hir_(unop('~', E) :: _) --> hir(E), [invert_bits(E.ty)].
hir_(unop(tuple_proj(Idx), E) :: _) --> hir(E), [mem_read_direct(Idx)].

hir_(let(X, Expr :: Ty) :: _) -->
    hir(Expr :: Ty),
    [store(local(X) :: Ty)].

hir_(var(X) :: Ty) --> [load(local(X) :: Ty)].

hir_(intr(Intr, Arg) :: _) -->
    hir(Arg),
    { syscall_number(Intr, N) },
    [syscall(N)].

hir_(if(Cond, Yes, No) :: _) -->
    { gensym('$if_end_', End) },
    { gensym('$if_else_', Else) },
    hir(Cond),
    [jmp_if_false(jmp_tgt(Else))],
    hir(Yes),
    [jmp(jmp_tgt(End))],
    [label(Else)],
    hir(No),
    [label(End)].

hir_(seq(A, B) :: _) -->
    hir(A),
    { A = _ :: ATy, type_size(ATy, NBytes) },
    ( { NBytes =:= 0 } -> [] ; [pop(NBytes)] ),
    hir(B).

hir_([] :: _) --> [].
hir_([E | Es] :: _) -->
    hir(E),
    hir(Es :: _).

hir_(call(::{tm: var(FnName), ty: _}, Args) :: _) -->
    !,
    hir(Args :: _),
    { length(ArgC, Args) },
    { FnLbl = FnName },
    [call(FnLbl, ArgC)].

hir_(call(Fn, Args) :: _) -->
    hir(Args :: _),
    hir(Fn),
    { length(ArgC, Args) },
    [call_indirect(ArgC)].

hir_(OTHER) --> { throw(error(unimplemented(hir_(OTHER)))) }.

gen_hir_from_item(::{tm: def{name:Name, params:_Params, ret_ty:_RetTy, body:Body}, ty: _}) :-
    phrase((
        [label(Name)],
        hir(Body),
        [ret]
    ), BodyHir),
    assertz( item_name_value(Name, hir(BodyHir)) ).

gen_hir_from_item(OTHER) :- throw(error(unimplemented(gen_hir_from_item(OTHER)))).

gen_hir_from_items(Items) :-
    abolish(item_name_value/2),
    gen_hir_from_items_(Items).

gen_hir_from_items_([]).
gen_hir_from_items_([Item | Items]) :-
    (   once(gen_hir_from_item(Item)) -> true
    ;
        throw(error('failure to gen HIR for item'(Item)))
    ),
    gen_hir_from_items_(Items).
