:- module(hir, [hir_instr/1, immediate/1, hir_instr_annotated/2]).
:- use_module(tycheck, [op(10, xfy, ::)]).
:- use_module(serde, [memspec_size/2]).
:- use_module(ty, [type_size/2]).

immediate(bool(true)).
immediate(bool(false)).
immediate(nat(_)).
immediate(int(_)).
immediate(jmp_tgt(_)).
immediate(local(_)).

% `+` indicates instr takes additional arguments.
% `-` indicates hir instr is same as lir instr.
hir_instr(+const(MemSpec, Imm)) :- memspec_size(MemSpec, _), immediate(Imm).
hir_instr(+load(local(_) :: Ty)) :- type_size(Ty, _).
hir_instr(+store(local(_) :: Ty)) :- type_size(Ty, _).
hir_instr(+jmp(jmp_tgt(_))).
hir_instr(+jmp_if_false(jmp_tgt(_))).
hir_instr(+call(_NArgs, label(_))).
hir_instr(+call_indirect(_NArgs)).
hir_instr(+syscall(_N)).
hir_instr(-halt). hir_instr(-nop).
hir_instr(-add(nat)). hir_instr(-add(int)).
hir_instr(-sub(nat)). hir_instr(-sub(int)).
hir_instr(-mul(nat)). hir_instr(-mul(int)).
hir_instr(-div(nat)). hir_instr(-div(int)).
hir_instr(-and). hir_instr(-or). hir_instr(-not).
hir_instr(+pop(_NBytes)).
hir_instr(-over(byte)). hir_instr(-over(short)). hir_instr(-over(word)). hir_instr(-over(qword)).
hir_instr(-rot(byte)).  hir_instr(-rot(short)).  hir_instr(-rot(word)).  hir_instr(-rot(qword)).
hir_instr(-gt(nat)). hir_instr(-gt(int)).
hir_instr(-lt(nat)). hir_instr(-lt(int)).
hir_instr(-eq(byte)). hir_instr(-eq(short)). hir_instr(-eq(word)). hir_instr(-eq(qword)).
hir_instr(-ret).

hir_instr_annotated(Instr, +Instr) :- hir_instr(+Instr).
hir_instr_annotated(Instr, -Instr) :- hir_instr(-Instr).
