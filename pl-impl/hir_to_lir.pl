:- module(hir_to_lir, [
    hir_to_lir/1,
    lir_instr/1,
    immediate_bytes//2
]).

:- use_module(tycheck, [op(10, xfy, ::)]).
:- use_module(serde, [unsigned_bytes//2, signed_bytes//2]).
:- use_module(hir, [hir_instr/1, hir_instr_annotated/2]).
:- use_module(ty, [type_size/2]).
:- use_module(tast_to_hir, [item_name_value/2]).
:- use_module(utils, [
    dcg_maplist//2,
    dcg_maplist//3,
    dupkeypairs_to_assoc/2,
    op(1050, xfy, else),
    else/2
]).
:- use_module(lir_gen_state,
    [ initial_lir_gen_state/1
    , run_with_lir_gen_state/2
    , emit_lir//1
    , op(700, fx, emit_from), 'emit_from'//1
    , current_fn//1
    , set_current_fn//1
    , label_index//2
    , label_is_current_index//1
    ]
).

:- use_module(library(assoc)).
:- use_module(library(clpfd)).


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

immediate_bytes_(bool(B), byte)         --> [N], { byte_bool(N, B) }.
immediate_bytes_(nat(N), MemSpec)       --> unsigned_bytes(MemSpec, N),     {type_memspec(nat, MemSpec)}.
immediate_bytes_(int(N), MemSpec)       -->   signed_bytes(MemSpec, N),     {type_memspec(int, MemSpec)}.
immediate_bytes_(jmp_tgt(Lbl), MemSpec) --> unsigned_bytes(MemSpec, Lbl),   {type_memspec(jmp_tgt, MemSpec)}.
immediate_bytes_(local(Local), MemSpec) --> unsigned_bytes(MemSpec, Local), {type_memspec(local, MemSpec)}.

byte_bool(0, false).
byte_bool(1, true).


:- dynamic generated_id/3.

gen_id(Scope, Symbol, Id) :- generated_id(Scope, Symbol, Id), !.
gen_id(Scope, Symbol, NewId) :-
    findall(I, generated_id(Scope, _, I), Ids),
    length(Ids, NewId),
    assertz( generated_id(Scope, Symbol, NewId) ).


%% lir(?AnnotatedHirInstruction)//.
%
% Must be run with `run_with_lir_gen_state` instead of `phrase`.
% lir(+const(byte, bool(Bool))) -->
%     { format('>>>>>>>>>>>101~n') },
% 	opcode(const(byte)),
%     { byte_bool(Byte, Bool) },
% 	emit_from [Byte],
% 	% emit_from serde:unsigned_bytes(byte, Byte),
%     { format('>>>>>>>>>>>105~n') },
%     {true}.
lir(+const(MemSpec, Imm)) -->
	opcode(const(MemSpec)),
	emit_from hir_to_lir:immediate_bytes(MemSpec, Imm).
lir(+load(Local :: Ty)) -->
	opcode(load_local),
	{ type_size(Ty, NBytes) },
	emit_from serde:unsigned_bytes(short, NBytes),
    current_fn(CurrentScope),
    { gen_id(CurrentScope, Local, Id) },
	emit_from hir_to_lir:immediate_bytes(short, Id).
lir(+store(Local :: Ty)) -->
	{ type_size(Ty, NBytes) },
	opcode(store_local),
	emit_from serde:unsigned_bytes(short, NBytes),
    current_fn(CurrentScope),
    { gen_id(CurrentScope, Local, Id) },
	emit_from hir_to_lir:immediate_bytes(short, Id).

lir(+jmp(jmp_tgt(Label))) -->
    label_index(Label, Index),
    opcode(jmp),
    emit_from serde:unsigned_bytes(word, Index).
lir(+jmp_if_false(jmp_tgt(Label))) -->
    label_index(Label, Index),
    opcode(jmp_if_false),
    emit_from serde:unsigned_bytes(word, Index).
lir(+call(NArgs, Label)) -->
    label_index(Label, Index),
    opcode(call),
    emit_from serde:unsigned_bytes(byte, NArgs),
    emit_from serde:unsigned_bytes(word, Index).

lir(+call_indirect(NArgs)) -->
    opcode(call_indirect),
    emit_from serde:unsigned_bytes(byte, NArgs).
lir(+syscall(N)) -->
    opcode(syscall),
    emit_from serde:unsigned_bytes(short, N).
lir(+pop(NBytes)) -->
    opcode(pop),
    emit_from serde:unsigned_bytes(short, NBytes).

lir(-Instr) --> opcode(Instr).

% lir(OTHER) --> { throw(error(unimplemented(lir(OTHER)))) }.
    
opcode(Instr) --> emit_lir([OpCode]), { lir_instr_opcode(Instr, OpCode) }.


%! hir_to_lir(-Lir).
%
% @see ./hir_to_lir.md for high-level overview of the translation process here.
hir_to_lir(Lir) :-
    findall(Name-Value, item_name_value(Name, Value), HirItems),
    run_with_lir_gen_state(hir_to_lir:gen_lir(HirItems), Lir).


:- discontiguous gen_lir//1.

gen_lir([]) --> [].
gen_lir([FnName-hir(Instrs)|Items]) -->
    set_current_fn(FnName),
    process_hir_instrs(Instrs),
    gen_lir(Items).


process_hir_instrs([]) --> [].
process_hir_instrs([label(Lbl) | Instrs]) --> !,
    label_is_current_index(Lbl),
    process_hir_instrs(Instrs).
process_hir_instrs([Instr | Instrs]) -->
    % Before we can pass `Instr` to `lir//1`, we need to annotate it.
    { hir_instr_annotated(Instr, InstrAnn) },
    lir(InstrAnn),
    process_hir_instrs(Instrs).


gen_lir([StaticName-static_text(Text) | Items]) -->
    label_is_current_index(StaticName),
    emit_from text_to_bytes(Text, _NBytes),
    gen_lir(Items).


text_to_bytes(Text, NBytes) -->
    { maplist(char_code, Text, Bytes) },
    { length(Bytes, NBytes) },
    Bytes.


:- use_module(library(plunit)).
:- begin_tests(hir_to_lir).

test('[const, const, add] roundtrip thru bytes', [blocked('infinite loop currently...')]) :-
    Hir = [const(qword, int(1)), const(qword, int(-1)), add(int)],
    phrase(hir_to_lir(Hir), Bytes),
    phrase(hir_to_lir(Guess), Bytes),
    Hir = Guess.

test('immediate_bytes: bool(true) from atom') :-
    phrase(immediate_bytes(byte, bool(true)), What),
    What == [1].

test('immediate_bytes: bool(false) from atom') :-
    phrase(immediate_bytes(byte, bool(false)), What),
    What == [0].

test('immediate_bytes: bool(true) from bytes') :-
    phrase(immediate_bytes(byte, bool(What)), [1]),
    What == true.

test('immediate_bytes: bool(false) from bytes') :-
    phrase(immediate_bytes(byte, bool(What)), [0]),
    What == false.

:- end_tests(hir_to_lir).