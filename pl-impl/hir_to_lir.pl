:- module(hir_to_lir, [
    hir_to_lir//1,
    lir//1,
    lir_instr/1,
    immediate_bytes//2
]).

:- use_module(tycheck, [op(10, xfy, ::)]).
:- use_module(serde, [unsigned_bytes//2, signed_bytes//2]).
:- use_module(hir, [hir_instr/1, hir_instr_annotated/2]).
:- use_module(ty, [type_size/2]).

lir_instr(halt). lir_instr(nop).
lir_instr(const(byte)).
lir_instr(const(short)).
lir_instr(const(word)).
lir_instr(const(qword)).
lir_instr(add(nat)). lir_instr(add(int)).
lir_instr(sub(nat)). lir_instr(sub(int)).
lir_instr(mul(nat)). lir_instr(mul(int)).
lir_instr(div(nat)). lir_instr(div(int)).
lir_instr(and). lir_instr(or). lir_instr(not).
lir_instr(pop). lir_instr(over). lir_instr(rot).
lir_instr(gt(nat)). lir_instr(gt(int)).
lir_instr(lt(nat)). lir_instr(lt(int)).
lir_instr(eq).
lir_instr(jmp). lir_instr(jmp_if_false). % [jmp/jmp_if_false, word(Label)]
lir_instr(call).                         % [call, byte(NArgs), word(Label)]
lir_instr(call_indirect).                % [call_indirect, byte(NArgs)]
lir_instr(ret).
lir_instr(load_local).  % [load_local,  short(NBytes), short(Index)]
lir_instr(store_local). % [store_local, short(NBytes), short(Index)]
lir_instr(syscall).     % [syscall, short(SyscallNumber)]

% Initialize the `lir_instr_opcode` database table.
:-  abolish(lir_instr_opcode/2),
    bagof(Instr, lir_instr(Instr), Instrs),
    forall(
        nth0(Index, Instrs, Instr_),
        assertz(lir_instr_opcode(Instr_, Index))
    ),
    ( length(Instrs, N), N > 256 ->
        throw(error(too_many_opcodes(actual(N), max(256))))
    ; true
    ).

type_memspec(bool, byte).
type_memspec(nat, qword). type_memspec(nat, word). type_memspec(nat, short). type_memspec(nat, byte).
type_memspec(int, qword). type_memspec(int, word). type_memspec(int, short). type_memspec(int, byte).
type_memspec(jmp_tgt, word).
type_memspec(local, short).

immediate_bytes(MemSpec, Imm) --> immediate_bytes_(Imm, MemSpec).

immediate_bytes_(bool(true), MemSpec)   --> unsigned_bytes(MemSpec, 1),     {type_memspec(bool, MemSpec)}.
immediate_bytes_(bool(false), MemSpec)  --> unsigned_bytes(MemSpec, 0),     {type_memspec(bool, MemSpec)}.
immediate_bytes_(nat(N), MemSpec)       --> unsigned_bytes(MemSpec, N),     {type_memspec(nat, MemSpec)}.
immediate_bytes_(int(N), MemSpec)       -->   signed_bytes(MemSpec, N),     {type_memspec(int, MemSpec)}.
immediate_bytes_(jmp_tgt(Lbl), MemSpec) --> unsigned_bytes(MemSpec, Lbl),   {type_memspec(jmp_tgt, MemSpec)}.
immediate_bytes_(local(Local), MemSpec) --> unsigned_bytes(MemSpec, Local), {type_memspec(local, MemSpec)}.


lir(+const(MemSpec, Imm)) -->
	opcode(const(MemSpec)),
	immediate_bytes(MemSpec, Imm).
lir(+load(Local :: Ty)) -->
	{ type_size(Ty, NBytes) },
	opcode(load_local),
	unsigned_bytes(short, NBytes),
	immediate_bytes(short, Local).
lir(+store(Local :: Ty)) -->
	{ type_size(Ty, NBytes) },
	opcode(store_local),
	unsigned_bytes(short, NBytes),
	immediate_bytes(short, Local).
lir(+jmp(Label)) -->
    opcode(jmp),
    unsigned_bytes(word, Label).
lir(+jmp_if_false(Label)) -->
    opcode(jmp_if_false),
    unsigned_bytes(word, Label).
lir(+call(NArgs, Label)) -->
    opcode(call),
    unsigned_bytes(byte, NArgs),
    unsigned_bytes(word, Label).
lir(+call_indirect(NArgs)) -->
    opcode(call_indirect),
    unsigned_bytes(byte, NArgs).
lir(+syscall(N)) -->
    opcode(syscall),
    unsigned_bytes(short, N).
lir(+pop(NBytes)) -->
    opcode(pop),
    unsigned_bytes(short, NBytes).

lir(-Instr) --> opcode(Instr).
    
opcode(Instr) -->
    [OpCode],
    { lir_instr_opcode(Instr, OpCode) }.

hir_to_lir([]) --> [].
hir_to_lir([Hir | Hirs]) -->
    { hir_instr_annotated(Hir, HirAnn) },
    lir(HirAnn),
    hir_to_lir(Hirs).

:- use_module(library(plunit)).
:- begin_tests(hir_to_lir).

test(const_const_add_roundtrip_thru_bytes) :-
    Hir = [const(byte, int(1)), const(byte, int(-1)), add(int)],
    once(phrase(hir_to_lir(Hir), Bytes)),
    findall(Guess, phrase(hir_to_lir(Guess), Bytes), Guesses),
    memberchk(Guess, Guesses).

:- end_tests(hir_to_lir).