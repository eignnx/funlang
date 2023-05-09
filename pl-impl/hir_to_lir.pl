:- op(1150, xfy, hir_lir).

'Load'(X :: void) hir_lir [].
'Load'(X :: bool) hir_lir [load(byte), local(X)].
'Load'(X :: int) hir_lir [load(word), local(X)].

'Store'(X :: void) hir_lir [].
'Store'(X :: bool) hir_lir [store(byte), local(X)].
'Store'(X :: int) hir_lir [store(word), local(X)].

'Jmp'(Lbl) hir_lir [jmp, label(Lbl)].
'JmpIfFalse'(Lbl) hir_lir [jmp_if_false, label(Lbl)].
'Label'(Lbl) hir_lir [].
'Pop' hir_lir [pop].

'Add'(byte) hir_lir [add(ubyte)].
'Add'(int) hir_lir [add(iword)].
'Add'(nat) hir_lir [add(uword)].

'Sub'(byte) hir_lir [sub(ubyte)].
'Sub'(int) hir_lir [sub(iword)].
'Sub'(nat) hir_lir [sub(uword)].

'Mul'(byte) hir_lir [mul(ubyte)].
'Mul'(int) hir_lir [mul(iword)].
'Mul'(nat) hir_lir [mul(uword)].

'Div'(byte) hir_lir [div(ubyte)].
'Div'(int) hir_lir [div(iword)].
'Div'(nat) hir_lir [div(uword)].

'Neg'(int) hir_lir [neg(iword)].

'And' hir_lir [and].
'Or' hir_lir [or].
'Not' hir_lir [not].
