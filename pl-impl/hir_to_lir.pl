:- module(hir_to_lir, [instr/1, lir//1, hir_to_lir//1]).
:- use_module(tycheck, [
    op(10, xfy, ::)
]).
:- use_module(serde, [
    unsigned_bytes//2,
    signed_bytes//2
]).
:- use_module(tast_to_hir, [
    type_size/2
]).

instr(halt). instr(nop).
instr(const(byte)).
instr(const(short)).
instr(const(word)).
instr(const(qword)).
instr(add(nat)). instr(add(int)).
instr(sub(nat)). instr(sub(int)).
instr(mul(nat)). instr(mul(int)).
instr(div(nat)). instr(div(int)).
instr(and). instr(or). instr(not).
instr(pop). instr(over). instr(rot).
instr(gt(nat)). instr(gt(int)).
instr(lt(nat)). instr(lt(int)).
instr(eq).
instr(jmp). instr(jmp_if_false). % [jmp/jmp_if_false, word(Label)]
instr(call).                     % [call, byte(NArgs), word(Label)]
instr(call_indirect).            % [call_indirect, byte(NArgs)]
instr(ret).
instr(load_local).  % [load_local,  short(NBytes), short(Index)]
instr(store_local). % [store_local, short(NBytes), short(Index)]
instr(syscall).     % [syscall, short(SyscallNumber)]

% Initialize the `instr_opcode` database table.
:-  abolish(instr_opcode/2),
    bagof(Instr, instr(Instr), Instrs),
    forall(
        nth0(Index, Instrs, Instr),
        assertz(instr_opcode(Instr, Index))
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


lir(const(MemSpec, Imm)) -->
	lir(const(MemSpec)),
	immediate_bytes(MemSpec, Imm).
lir(load(Local :: Ty)) -->
	{ type_size(Ty, NBytes) },
	lir(load_local),
	unsigned_bytes(short, NBytes),
	immediate_bytes(short, Local).
lir(store(Local :: Ty)) -->
	{ type_size(Ty, NBytes) },
	lir(store_local),
	unsigned_bytes(short, NBytes),
	immediate_bytes(short, Local).
lir(jmp(Label)) -->
    lir(jmp),
    unsigned_bytes(word, Label).
lir(jmp_if_false(Label)) -->
    lir(jmp_if_false),
    unsigned_bytes(word, Label).
lir(call(NArgs, Label)) -->
    lir(call),
    unsigned_bytes(byte, NArgs),
    unsigned_bytes(word, Label).
lir(call_indirect(NArgs)) -->
    lir(call_indirect),
    unsigned_bytes(byte, NArgs).
lir(syscall(N)) -->
    lir(syscall),
    unsigned_bytes(short, N).

lir(Instr) -->
    [OpCode],
    { instr_opcode(Instr, OpCode) }.


hir_to_lir([]) --> [].
hir_to_lir([Hir | Hirs]) -->
    lir(Hir),
    hir_to_lir(Hirs).