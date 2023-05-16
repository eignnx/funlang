% Serialize/Deserialize (serde)
:- module(serde, [memspec_size/2, unsigned_bytes//2, signed_bytes//2]).
:- use_module(library(clpfd)).
:- use_module(library(crypto), [hex_bytes/2]).

:- det(memspec_size/2).
memspec_size(byte, 1).
memspec_size(short, 2).
memspec_size(word, 4).
memspec_size(qword, 8).

:- det(unsigned_bytes//2).
unsigned_bytes(MemSpec, U) -->
    { memspec_size(MemSpec, NBytes) },
    { natural_bytes(U, Bs, NBytes) },
    Bs.

signed_bytes(MemSpec, S) -->
    { memspec_size(MemSpec, NBytes) },
    { integer_bytes(S, Bs, NBytes) },
    Bs.
    
int_bounds(NBytes, Hi, Lo) :-
    Hi #= 256^NBytes // 2 - 1,
    Lo #= -(256^NBytes) // 2.

integer_bytes(I, Bs, NBytes) :-
    int_bounds(NBytes, Hi, Lo),
    I in Lo..Hi,
    zcompare(Cmp, I, 0),
    integer_bytes(Cmp, I, Bs, NBytes).
integer_bytes(<, I0, Bs, NBytes) :-
    I #= I0 mod 256^NBytes, % Essentially do bit-inversion.
    natural_bytes(I, Bs, NBytes).
integer_bytes(>, I, Bs, NBytes) :-
    natural_bytes(I, Bs, NBytes).
integer_bytes(=, I, Bs, NBytes) :-
    natural_bytes(I, Bs, NBytes).

:- det(natural_bytes/3).
natural_bytes(N, Bytes, NBytes) :-
    length(Bytes, NBytes),
    once(phrase(base256(N), Bytes)).

zeros --> [] | [0], zeros.

base256(N) -->
    zeros,
    { zcompare(Cmp, N, 0) },
    base256_(Cmp, N).

base256_(=, 0) --> [].
base256_(>, N) -->
    { Byte in 0..255 },
    { Byte #= N mod 256 },
    { Next #= N div 256 },
    { zcompare(Cmp, Next, 0) },
    base256_(Cmp, Next),
    [Byte].
base256_(<, N) --> { throw(error(negative_int_arg_to_base256(N))) }.
