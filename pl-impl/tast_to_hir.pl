
hir(lit(int(I)) :: _) --> ['Load'('VInt'(I))].

hir(lit(bool(B)) :: _) --> ['Load'('VBool'(B))].

hir(add(A, B) :: _) --> hir(A), hir(B), ['Add'].

hir(let(X, Expr) :: _) --> hir(Expr), ['Store'(X)].

hir(var(X) :: _) --> ['Load'(X)].

hir(if(Cond, Yes, No) :: _) -->
    hir(Cond),
    ['JmpIfFalse'(Else)],
    hir(Yes),
    ['Jmp'(End)],
    ['Label'(Else)],
    hir(No),
    ['Label'(End)].

hir(seq(A, B) :: _) -->
    hir(A),
    ( { A = _ :: ATy, zero_sized(ATy) } -> [] ; ['Pop'] ),
    hir(B).

hir(Other) --> { throw(error(unimplemented(hir(Other)))) }.


zero_sized(void).

