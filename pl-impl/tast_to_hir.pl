
hir(lit(int(I)) :: _) --> ['Load'('VInt'(I))].

hir(lit(bool(B)) :: _) --> ['Load'('VBool'(B))].

hir(add(A, B) :: Ty) --> hir(A), hir(B), ['Add'(Ty)].
hir(sub(A, B) :: Ty) --> hir(A), hir(B), ['Sub'(Ty)].
hir(mul(A, B) :: Ty) --> hir(A), hir(B), ['Mul'(Ty)].
hir(div(A, B) :: Ty) --> hir(A), hir(B), ['Div'(Ty)].
hir(and(A, B) :: _) --> hir(A), hir(B), ['And'].
hir(or(A, B) :: _) --> hir(A), hir(B), ['Or'].

hir(neg(A) :: Ty) --> hir(A), ['Neg'(Ty)].
hir(not(A) :: _) --> hir(A), ['Not'].

hir(let(X, Expr) :: _) --> hir(Expr), ['Store'(X)].

hir(var(X) :: Ty) --> ['Load'(X :: Ty)].

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

