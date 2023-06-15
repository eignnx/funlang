:- module(hir_to_lir, [
    hir_to_lir//0,
    lir_instr/1,
    immediate_bytes//2
]).

:- use_module(tycheck, [op(10, xfy, ::)]).
:- use_module(serde, [unsigned_bytes//2, signed_bytes//2]).
:- use_module(hir, [hir_instr/1, hir_instr_annotated/2]).
:- use_module(ty, [type_size/2]).
:- use_module(utils, [
    dcg_maplist//2,
    dcg_maplist//3,
    dupkeypairs_to_assoc/2,
    op(1050, xfy, else),
    else/2
]).
:- use_module(tast_to_hir, [item_name_value/2]).

:- use_module(library(assoc)).


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


%% lir(?AnnotatedHirInstruction, +CurrentScope)//.
%
lir(+const(MemSpec, Imm), _, _) -->
	opcode(const(MemSpec)),
	immediate_bytes(MemSpec, Imm).
lir(+load(Local :: Ty), _, CurrentScope) -->
	{ type_size(Ty, NBytes) },
	opcode(load_local),
	unsigned_bytes(short, NBytes),
    { gen_id(CurrentScope, Local, Id) },
	immediate_bytes(short, Id).
lir(+store(Local :: Ty), _, CurrentScope) -->
	{ type_size(Ty, NBytes) },
	opcode(store_local),
	unsigned_bytes(short, NBytes),
    { gen_id(CurrentScope, Local, Id) },
	immediate_bytes(short, Id).

lir(+jmp(Label), LabelAssoc, _) -->
    { get_assoc(Label, LabelAssoc, Index) else throw(error(unknown_key_in_assoc(Label), _)) },
    opcode(jmp),
    unsigned_bytes(word, Index).
lir(+jmp_if_false(Label), LabelAssoc, _) -->
    { get_assoc(Label, LabelAssoc, Index) else throw(error(unknown_key_in_assoc(Label), _)) },
    opcode(jmp_if_false),
    unsigned_bytes(word, Index).
lir(+call(NArgs, Label), LabelAssoc, _) -->
    { get_assoc(Label, LabelAssoc, Index) else throw(error(unknown_key_in_assoc(Label), _)) },
    opcode(call),
    unsigned_bytes(byte, NArgs),
    unsigned_bytes(word, Index).

lir(+call_indirect(NArgs), _, _) -->
    opcode(call_indirect),
    unsigned_bytes(byte, NArgs).
lir(+syscall(N), _, _) -->
    opcode(syscall),
    unsigned_bytes(short, N).
lir(+pop(NBytes), _, _) -->
    opcode(pop),
    unsigned_bytes(short, NBytes).

lir(-Instr, _, _) --> opcode(Instr).
    
opcode(Instr) --> [OpCode], { lir_instr_opcode(Instr, OpCode) }.


%! hir_to_lir//
%
% @see ./hir_to_lir.md for high-level overview of the translation process here.
hir_to_lir -->
    { findall(Name-Value, item_name_value(Name, Value), NamesValues) },
    { pairs_keys_values(NamesValues, Names, Values) },
    dcg_maplist(gen_lir, Names, Values).

gen_lir(FnName, hir(Hir)) -->
    dcg_maplist([Instr]>>lir(Instr, _, FnName), Hir).

gen_lir(_StaticName, static_text(Text)) -->
    text_to_bytes(Text).

text_to_bytes(Text) -->
    { maplist(char_code, Text, Bytes) },
    Bytes.


    % { phrase(first_pass(Hir), SymbolRecords) },
    % { format('HirNoLabels = '), portray_clause(HirNoLabels) },
    % second_pass(HirNoLabels, SymbolRecords).


% first_pass([], [], _) --> [].

% first_pass([label(Label), Instr | Hir], HirNoLabels, Index) --> !,
%     [Label-Index], % Save the label-index pair...
%     { var(Label) -> gensym(lbl_, UniqSym), Label = UniqSym ; true },
%     first_pass([Instr | Hir], HirNoLabels, Index). % ...process the rest as if the label wasn't there.

% first_pass([const(ptr, static_text(Text)) | Hir], HirNoLabels, Index0) --> !,
%     { gensym('$static_text_', UniqSym) },
%     [UniqSym-TextStart],
%     { Index is Index0 + 1 },
%     first_pass([const(qword, nat(TextStart)) | Hir], HirNoLabels0, Index),
%     { length(HirNoLabels0, TextStart) },
%     { maplist([Char, #(Code)]>>char_code(Char, Code), Text, Codes) },
%     { append(HirNoLabels0, Codes, HirNoLabels) }.

% first_pass([Instr | Hir], [Instr | HirNoLabels], Index0) -->
%     { Index is Index0 + 1 },
%     first_pass(Hir, HirNoLabels, Index).

    
% second_pass([], _LabelAssoc) --> [].
% second_pass([#(Code) | Hirs], LabelAssoc) --> !, [Code], second_pass(Hirs, LabelAssoc).
% second_pass([Hir | Hirs], LabelAssoc) -->
%     { hir_instr_annotated(Hir, HirAnn) },
%     lir(HirAnn, LabelAssoc),
%     second_pass(Hirs, LabelAssoc).

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