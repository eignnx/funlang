:- module(tast_to_hir, [hir//1]).
:- use_module(ty, [type_size/2]).

:- op(10, xfy, ::).

hir(::{tm:Tm, ty:Ty}) --> hir_(Tm::Ty).

:- det(hir_//1).

hir_(lit(int(I)) :: _) --> [const(qword, int(I))].
hir_(lit(nat(I)) :: _) --> [const(qword, nat(I))].
hir_(lit(bool(B)) :: _) --> [const(byte, bool(B))].

hir_(binop(+, A, B) :: Ty) --> hir(A), hir(B), [add(Ty)].
hir_(binop(-, A, B) :: Ty) --> hir(A), hir(B), [sub(Ty)].
hir_(binop(*, A, B) :: Ty) --> hir(A), hir(B), [mul(Ty)].
hir_(binop(/, A, B) :: Ty) --> hir(A), hir(B), [div(Ty)].
hir_(binop(and, A, B) :: _) --> hir(A), hir(B), [and].
hir_(binop(or, A, B) :: _) --> hir(A), hir(B), [or].
hir_(binop(xor, A, B) :: _) --> hir(A), hir(B), [over, over, or, rot, rot, and, not, and].
hir_(binop('>', A, B) :: Ty) --> hir(A), hir(B), [gt(Ty)].
hir_(binop('<', A, B) :: Ty) --> hir(A), hir(B), [lt(Ty)].
hir_(binop('==', A, B) :: _) --> hir(A), hir(B), [eq].
hir_(binop('!=', A, B) :: _) --> hir(A), hir(B), [eq, not].
hir_(binop('>=', A, B) :: Ty) --> hir(A), hir(B), [over, over, gt(Ty), eq, or].
hir_(binop('<=', A, B) :: Ty) --> hir(A), hir(B), [over, over, lt(Ty), eq, or].

hir_(let(X, Expr :: Ty) :: _) -->
    hir(Expr :: Ty),
    [store(local(X) :: Ty)].

hir_(var(X) :: Ty) --> [load(local(X) :: Ty)].

hir_(if(Cond, Yes, No) :: _) -->
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

hir_(def{name:Name, params:_Params, ret_ty:_RetTy, body:Body} :: _) -->
    [label(Name)],
    hir(Body),
    [ret].