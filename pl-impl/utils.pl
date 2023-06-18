:- module(utils,
    [ statefully/2
    , state//1
    , op(950, xfx, before_after), before_after//2
    , dcg_maplist//2
    , dcg_maplist//3
    , dupkeypairs_to_assoc/2
    , op(1050, xfy, else), else/2
    , get_or_insert_assoc/4
    ]
).

%% statefully(+DcgBody, ?InitialState -> ?FinalState).
%
% Use this to run a stateful program written as a DCG rule.
% Use `state//1` to view and modify the state inside the DCG rule.
statefully(DcgBody, State0->State) :-
    phrase(DcgBody, [State0], [State]).

state(S), [S] --> !, [S].

(S0 before_after S), [S] --> [S0].

%% uniq_keysort(+PairListWithPossiblyDuplicateKeys, -KeySortedPairList).
%
% This is just like `keysort` except it removes duplicate keys.
uniq_keysort(DupKeyPairs, SortedUniqPairs) :-
    sort(1, @<, DupKeyPairs, SortedUniqPairs).


%% dupkeypairs_to_assoc(+PairListWithPossiblyDuplicateKeys, -Assoc).
%
dupkeypairs_to_assoc(DupKeyPairs, Assoc) :-
    uniq_keysort(DupKeyPairs, SortedUniqPairs),
    ord_list_to_assoc(SortedUniqPairs, Assoc).

(Goal else Alternative) :-
    call(Goal) *-> true ; call(Alternative).


:- non_terminal(dcg_maplist//2).

%% dcg_maplist(+Arity1DcgBody, ?List)//.
%
dcg_maplist(DcgBody, Xs, S, S0) :-
    dcg_maplist_(Xs, S, S0, DcgBody).

:- non_terminal(dcg_maplist_//2).

dcg_maplist_([], S, S, _DcgBody).
dcg_maplist_([X|Xs], S0, S, DcgBody) :-
    call(DcgBody, X, S0, S1),
    dcg_maplist_(Xs, S1, S, DcgBody).


:- non_terminal(dcg_maplist//3).

%% dcg_maplist(+Arity2DcgBody, ?List1, ?List2)//.
%
dcg_maplist(DcgBody, Xs, Ys, S, S0) :-
    dcg_maplist_(Xs, Ys, S, S0, DcgBody).

:- non_terminal(dcg_maplist_//3).

dcg_maplist_([], [], S, S, _DcgBody).
dcg_maplist_([X|Xs], [Y|Ys], S0, S, DcgBody) :-
    call(DcgBody, X, Y, S0, S1),
    dcg_maplist_(Xs, Ys, S1, S, DcgBody).


:- use_module(library(assoc)).

get_or_insert_assoc(Key, Assoc0, Value, Assoc) :-
    (   get_assoc(Key, Assoc0, Value) ->
        Assoc = Assoc0
    ;
        put_assoc(Key, Assoc0, Value, Assoc)
    ).
    
    
    