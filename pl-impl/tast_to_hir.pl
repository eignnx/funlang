:- module(tast_to_hir, [hir//1]).
:- use_module(tycheck, [
    op(10, xfy, ::)
]).
:- use_module(ty, [type_size/2]).

:- det(hir//1).

hir(lit(int(I)) :: _) --> [const(qword, int(I))].
hir(lit(nat(I)) :: _) --> [const(qword, nat(I))].
hir(lit(bool(B)) :: _) --> [const(byte, bool(B))].

hir(binop(+, A, B) :: Ty) --> hir(A), hir(B), [add(Ty)].
hir(binop(-, A, B) :: Ty) --> hir(A), hir(B), [sub(Ty)].
hir(binop(*, A, B) :: Ty) --> hir(A), hir(B), [mul(Ty)].
hir(binop(/, A, B) :: Ty) --> hir(A), hir(B), [div(Ty)].
hir(binop(and, A, B) :: _) --> hir(A), hir(B), [and].
hir(binop(or, A, B) :: _) --> hir(A), hir(B), [or].
hir(binop(xor, A, B) :: _) --> hir(A), hir(B), [over, over, or, rot, rot, and, not, and].
hir(binop('>', A, B) :: Ty) --> hir(A), hir(B), [gt(Ty)].
hir(binop('<', A, B) :: Ty) --> hir(A), hir(B), [lt(Ty)].
hir(binop('==', A, B) :: _) --> hir(A), hir(B), [eq].
hir(binop('!=', A, B) :: _) --> hir(A), hir(B), [eq, not].
hir(binop('>=', A, B) :: Ty) --> hir(A), hir(B), [over, over, gt(Ty), eq, or].
hir(binop('<=', A, B) :: Ty) --> hir(A), hir(B), [over, over, lt(Ty), eq, or].

hir(let(X, Expr :: Ty) :: _) -->
    hir(Expr :: Ty),
    [store(local(X) :: Ty)].

hir(var(X) :: Ty) --> [load(local(X) :: Ty)].

hir(if(Cond, Yes, No) :: _) -->
    hir(Cond),
    [jmp_if_false(jmp_tgt(Else))],
    hir(Yes),
    [jmp(jmp_tgt(End))],
    [label(Else)],
    hir(No),
    [label(End)].

hir(seq(A, B) :: _) -->
    hir(A),
    { A = _ :: ATy, type_size(ATy, NBytes) },
    ( { NBytes =:= 0 } -> [] ; [pop(NBytes)] ),
    hir(B).
