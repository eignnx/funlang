:- module(utils,
    [ statefully/2
    , state//1
    , op(950, xfx, before_after), before_after//2
    % , dcg_maplist//3
    % , dcg_maplist//2
    , dupkeypairs_to_assoc/2
    , op(1050, xfy, else), else/2
    ]
).

:- op(950, xfx, before_after).
:- op(1050, xfy, else).

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
