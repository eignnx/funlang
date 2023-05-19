% Serialize/Deserialize (serde)
:- module(serde, [memspec_size/2, unsigned_bytes//2, signed_bytes//2]).
:- use_module(library(clpfd)).
:- use_module(library(crypto), [hex_bytes/2]).

memspec_size(byte, 1).
memspec_size(short, 2).
memspec_size(word, 4).
memspec_size(qword, 8).

unsigned_bytes(MemSpec, U) -->
    { memspec_size(MemSpec, NBytes) },
    { length(Bs, NBytes) },
    Bs,
    { natural_bytes(U, Bs, NBytes) }.

signed_bytes(MemSpec, S) -->
    { memspec_size(MemSpec, NBytes) },
    { length(Bs, NBytes) },
    Bs,
    { once(integer_bytes(S, Bs, NBytes)) }.
    
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

natural_bytes(N, Bytes, NBytes) :-
    length(Bytes, NBytes),
    be_bytes_nat(Bytes, N).

%% Big-endian Bytes related to a Natural Number.
be_bytes_nat(Bytes, N) :-
    (   var(Bytes) ->
    	be_bytes_nat(BytesRev, N),
    	reverse(BytesRev, Bytes)
    ;
    	reverse(BytesRev, Bytes),
        once(le_bytes_nat(BytesRev, N))
    ).

le_bytes_nat(Bytes, N) :-
    ( var(N) ->
        bytes_to_int(Bytes, N)
    ;
    	zcompare(Cmp, N, 0),
        int_to_bytes(Cmp, N, Bytes)
    ).

% :- mode(+Bytes, -N).
bytes_to_int(Zeros, 0) :- maplist(#=(0), Zeros).
bytes_to_int([Byte | Bytes], N) :-
    Byte in 0..255,
    N #= 256*N0 + Byte,
    bytes_to_int(Bytes, N0).

% :- mode(+N, -Bytes).
int_to_bytes(=, 0, Zeros) :- maplist(#=(0), Zeros).
int_to_bytes(>, N, [Byte|Bytes]) :-
    N #> 0,
    Byte #= N mod 256,
    Next #= N div 256,
    zcompare(Cmp, Next, 0),
    int_to_bytes(Cmp, Next, Bytes).


:- use_module(library(plunit)).
:- begin_tests(serde).

test(round_trip_unsigned_word_123) :-
    phrase(unsigned_bytes(word, 123), Bytes),
    phrase(unsigned_bytes(word, Guess), Bytes),
    assertion(Guess =:= 123).

test(round_trip_signed_word_neg_123_to_from_bytes) :-
    phrase(signed_bytes(word, -123), Bytes),
    phrase(signed_bytes(word, Guess), Bytes),
    assertion(Guess =:= -123).

test(round_trip_unsigned_word_to_from_word) :-
    phrase(unsigned_bytes(word, N), [1,2,3,4]),
    phrase(unsigned_bytes(word, N), Guess),
    assertion(Guess = [1,2,3,4]).

test(round_trip_signed_neg_word_to_from_word) :-
    phrase(signed_bytes(word, N), [255,2,3,4]),
    phrase(signed_bytes(word, N), Guess),
    assertion(Guess = [255,2,3,4]).

test(round_trip_signed_pos_word_to_from_word) :-
    phrase(signed_bytes(word, N), [0,1,2,3]),
    phrase(signed_bytes(word, N), Guess),
    assertion(Guess = [0,1,2,3]).


:- end_tests(serde).