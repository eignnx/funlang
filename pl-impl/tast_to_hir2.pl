
:- det(hir//1).

% Loading an "immediate" value requires value be stored in
% subsequent instruction's location.
hir(lit(int(I)) :: _) --> ['Const'('Word'), 'Word'(I)].
hir(lit(bool(B)) :: _) --> ['Const'('Byte'), 'Byte'(B)].

hir(add(A, B) :: Ty) --> hir(A), hir(B), ['Add'(Ty)].
hir(and(A, B) :: _) --> hir(A), hir(B), ['And'].
hir(or(A, B) :: _) --> hir(A), hir(B), ['Or'].

hir(let(X, Expr :: Ty) :: _) -->
    hir(Expr :: Ty),
    ['Store'(Ty), 'Local'(X)].

hir(var(X) :: Ty) --> ['Load'(Ty), 'Local'(X)].

hir(if(Cond, Yes, No) :: _) -->
    hir(Cond),
    ['JmpIfFalse', 'JmpTgt'(Else)],
    hir(Yes),
    ['Jmp', 'JmpTgt'(End)],
    ['Label'(Else)],
    hir(No),
    ['Label'(End)].

hir(seq(A, B) :: _) -->
    hir(A),
    { A = _ :: ATy, ty_size(ATy, N) },
    ( { N =:= 0 } -> [] ; ['Pop'(N)] ),
    hir(B).


%% Size in bytes.
ty_size(void, 0).
ty_size(int, 8). % TODO: Make arch independent?
ty_size(bool, 1).

