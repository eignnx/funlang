:- module(lir_gen_state,
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

:- use_module(openmap, [openmap_key_value/3]).
:- use_module(utils,
    [ statefully/2
    , state//1
    , op(950, xfx, before_after), before_after//2
    ]).

:- use_module(library(clpfd)).


initial_lir_gen_state(
    state{ lir_so_far: []
         , byte_index: 0
         , current_fn: '$no_current_fn_yet'
         , lbls_idxs: _NewOpenmap
         }
).

%! run_with_lir_gen_state(+DcgBody, -Lir).
%
run_with_lir_gen_state(DcgBody, Lir) :-
    initial_lir_gen_state(State0),
    phrase(DcgBody, [State0], [State]),
    Lir = State.lir_so_far.

%%%%%%%%%%%%%%% St.lir_so_far, St.byte_index %%%%%%%%%%%%%%%%%%%%%%%%

emit_lir(LirInstrs) -->
    S0 before_after S,
    {
        append(S0.lir_so_far, LirInstrs, NewLir),
        length(LirInstrs, NBytes),
        NewByteIndex #= S0.byte_index + NBytes,
        S = S0.put(_{ lir_so_far: NewLir
                    , byte_index: NewByteIndex
                    })
    }.

%! emit_from(+DcgBody)//.
%
% Put this in front of a DCG nonterminal that normally would
% output lir instructions as a list.
(emit_from DcgBody) -->
    { phrase(DcgBody, LirInstrs, _) },
    emit_lir(LirInstrs).

%%%%%%%%%%%%%%%%%%%%%%%%% St.current_fn %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

current_fn(St.current_fn) --> state(St).

set_current_fn(NewFnName) -->
    S0 before_after S,
    {
        S = S0.put(_{ current_fn: NewFnName })
    }.

%%%%%%%%%%%%%%%%%%%%%%%%% St.lbls_idxs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

label_index(Lbl, Idx) -->
    state(S),
    { openmap_key_value(S.lbls_idxs, Lbl, Idx) }.

label_is_current_index(Lbl) -->
    state(S),
    { openmap_key_value(S.lbls_idxs, Lbl, S.byte_index) }.
