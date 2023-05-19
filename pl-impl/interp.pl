:- module(interp, [exe//1]).

:- use_module(hir_to_lir, [hir_to_lir//1, immediate_bytes//2]).
:- use_module(serde, [memspec_size/2]).

:- use_module(library(clpfd)).


:- op(1199, xfy, do).
:- op(1198, xfy, where).
:- op(1106, xfy, --).
:- op(1106, fx, --).

term_expansion((Head do ( Before -- After ) where Ops), (Head, After --> Before, { Ops })).
term_expansion((Head do ( -- After ) where Ops), (Head, After --> { Ops })).
term_expansion((Head do ( Before -- After )), (Head, After --> Before)).


bool(X) --> immediate_bytes(byte, bool(X)).
num(Value, nat) --> immediate_bytes(qword, nat(Value)).
num(Value, int) --> immediate_bytes(qword, int(Value)).
bytes(Bytes, NBytes) --> { length(Bytes, NBytes) }, Bytes.

and(true, true, true) :- !.
and(_, _, false).

or(true, _, true) :- !.
or(_, true, true) :- !.
or(_, _, false).

not(true, false).
not(false, true).


exe(over(MemSpec))
    do (
        bytes(A, N), bytes(B, N)
        --
        bytes(B, N), bytes(A, N), bytes(B, N)
    ) where
        memspec_size(MemSpec, N).

exe(rot(MemSpec))
    do (
        bytes(A, N), bytes(B, N), bytes(C, N)
        --
        bytes(C, N), bytes(A, N), bytes(B, N)
    ) where
        memspec_size(MemSpec, N).

exe(and) do ( bool(A), bool(B) -- bool(C) ) where
    and(A, B, C).

exe(or) do ( bool(A), bool(B) -- bool(C) ) where
    or(A, B, C).

exe(not) do ( bool(A) -- bool(B) ) where not(A, B).

exe(const(MemSpec, Imm)) do ( -- Bytes ) where
    phrase(immediate_bytes(MemSpec, Imm), Bytes).

exe(add(Ty)) do ( num(A, Ty), num(B, Ty) -- num(C, Ty) ) where
    C #= A + B.

exe(sub(Ty)) do ( num(A, Ty), num(B, Ty) -- num(C, Ty) ) where
    C #= A - B.

exe(mul(Ty)) do ( num(A, Ty), num(B, Ty) -- num(C, Ty) ) where
    C #= A div B.

exe(div(Ty)) do ( num(A, Ty), num(B, Ty) -- num(C, Ty) ) where
    C #= A div B.

exe(gt(Ty)) do ( num(A, Ty), num(B, Ty) -- bool(C) ) where
    A #> B -> C = true ; C = false.

:- use_module(library(plunit)).
:- begin_tests(interp).

test(and) :-
    once(phrase(exe(and), [1, 0], EndStack)),
    assertion(EndStack = [0 | _]).

test(or) :-
    once(phrase(exe(or), [1, 0], EndStack)),
    assertion(EndStack = [1 | _]).

test(not) :-
    once(phrase(exe(not), [1], EndStack)),
    assertion(EndStack = [0]).

test(const_short_int) :-
    once(phrase(exe(const(short, int(123))), [], EndStack)),
    assertion(EndStack = [0, 123]).

test(add_nat) :-
    phrase(num(1, nat), One),
    phrase(num(2, nat), Two),
    once(phrase(num(3, nat), Three)),
    append(One, Two, OneTwo),
    once(phrase(exe(add(nat)), OneTwo, EndStack)),
    assertion(EndStack = Three).

test(add_int) :-
    phrase((num(-50, int), num(100, int)), AB),
    once(phrase(num(50, int), C)),
    once(phrase(exe(add(int)), AB, EndStack)),
    assertion(EndStack = C).

test(gt_nat_true) :-
    phrase((num(2, nat), num(1, nat)), TwoOne),
    once(phrase(exe(gt(nat)), TwoOne, EndStack)),
    assertion(EndStack = [1]).

test(gt_nat_false) :-
    phrase((num(1, nat), num(2, nat)), OneTwo),
    once(phrase(exe(gt(nat)), OneTwo, EndStack)),
    assertion(EndStack = [0]).

:- end_tests(interp).