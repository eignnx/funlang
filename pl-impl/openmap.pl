:- module(openmap, [
    openmap_key_value/3,
    om_key_val/3,
    openmap_definite_key_value/3
]).

:- use_module(library(error), [
    instantiation_error/1,
    uninstantiation_error/1
]).

%! <module> openmap.pl
%
% The problem with a `library(assoc)` association, pairlist, or rbtree
% is that they assume all the information in the map is known ahead of
% time. But sometimes key-value mappings only become known later in time.
% 
% An openmap doesn't assume that all information contained in the map
% is known at any particular time.
%
% ## Example
%
% For example, suppose `Ages` is an openmap mapping people's names to
% their ages. Maybe my friend queries `Ages` for the age of a person
% named `lilly` by running the following code:
%
% ```prolog
% ?- openmap_key_value(Ages, lilly, LillysAge).
% ```
% 
% If there exists a mapping for the key `lilly` at the time the query
% is made, her age will be bound to `LillysAge` and that's that.
%
% But if no key `lilly` is in the map at the time of query, then the
% "dummy" mapping `lilly-FreshVar` will be inserted into the openmap
% and `LillysAge` will be unified with `FreshVar`.
%
% My friend can now use `LillysAge` as an uninstantiated variable for now
% and will only later learn lilly's actual age.
%
% That is, if I later assert the following:
%
% ```prolog
% ?- openmap_key_value(Ages, lilly, 26).
% ```
%
% ...then my friend's `LillysAge` variable will become instantiated to the
% number `26`.
%
% ## Representation of Openmaps
%
% An `openmap` is an unbound variable, or a term `[K-V | M]` where
% `K` is ground term acting as the key for `V`, `K`'s associated value,
% and `M` is another `openmap` (again, possibly an unbound variable).
%
% To create an empty `openmap`, just create a fresh variable.
%
% To query **or update** an `openmap`, use `openmap_key_value/3`. 
% 
% Note: all of the keys of an openmap must be ground terms. They ought
% not be variables or contain variables. I haven't entirely thought
% through why, but it seems like a bad idea. You'll probably get
% unexpected results.
%

%! openmap_key_value(?Openmap, ++Key, ?Value) is det.
%
% Succeeds binding `Value` to the (possibly currently unknown) value
% associated with `Key`.
%
% If you try to associate one key with two values, an uninstantiation
% error is thrown. Note that in that case the query does not fail, but
% throws an exception! To see why failure is not an option, here's an
% example. Assume that the predicate *does* fail instead of error:
%
% ```prolog
% ?- openmap_key_value(O, lilly, 26).
%
% ?- openmap_key_value($O, lilly, Age), % Succeeds, as expected.
%    format('Lilly is ~w.~n', [Age]).
%  Lilly is 26.
%  true. % We trust that Prolog reports only The Truth.
%
% ?- openmap_key_value($O, lilly, 62). % Uh-oh, she can't be 26
%                                      % and 62 at the same time!
%    false. % We use the assumption that the query fails instead
%           % of erroring.
% ```
%
% Now let's try swapping the first and third queries. Because we expect
% conjunction ('and'ing) to be commutative, we should see the same results
% regardless of the ordering of goals.
%
% ```prolog
% ?- openmap_key_value(O, lilly, 62).
% ?- openmap_key_value($O, lilly, Age), % Succeeds, as expected.
%    format('Lilly is ~w.~n', [Age]).
%  Lilly is 62.
%  true. % Wait, Prolog is telling us conflicting things!
%
% ?- openmap_key_value($O, lilly, 26).
%    false.
% ```
%
% For this reason, we shouldn't ever expect `openmap_key_val/3` to fail.
% If you encounter an exception like `uninstantiation_error(lilly-[26, 62])`
% this is an indication that your program's logic is flawed (no offense).
%
openmap_key_value([K1-V1 | M], K2, V2) :-
    ground(K2) ->
        openmap_key_value_([K1-V1 | M], K2, V2)
    ;
        instantiation_error(K2).

openmap_key_value_([K-V1 | _], K, V2) :- !,
    ( V1 = V2 -> true ; uninstantiation_error(K-[V1,V2]) ).
openmap_key_value_([_-_ | M], K, V) :-
    openmap_key_value_(M, K, V).


%! om_key_val(O, K, V).
%
% An abbreviation for `openmap_key_value/3`.
om_key_val(O, K, V) :- openmap_key_value(O, K, V).


%! openmap_definite_key_value(?Openmap, ++Key, ?Value) is semidet.
%
% Succeeds when `Key-Value` is *definitely* (i.e. without a doubt) a mapping in
% `Openmap`. If this predicate fails, it means no determination can be made *at
% this time*, but it's *possible* the mapping *will* be definite once more is known
% about the contents of `Openmap`.
openmap_definite_key_value(O, _, _) :- var(O), !, false.
openmap_definite_key_value([K1-V1 | _], K2, V2) :-
    ground([K1, V1, K2]),
    K1 = K2, V1 = V2, !.
openmap_definite_key_value([_-_ | M], K, V) :-
    openmap_definite_key_value(M, K, V).


:- use_module(library(plunit)).
:- begin_tests(openmap).

test('openmap_key_value : single element', [true(A == 1)]) :-
    openmap_key_value(O, a, 1),
    openmap_key_value(O, a, A).

test('openmap_key_value : two element openmap', [true((A==1, B==2))]) :-
    openmap_key_value(O, a, 1),
    openmap_key_value(O, b, 2),
    openmap_key_value(O, a, A),
    openmap_key_value(O, b, B).

test('openmap_key_value : uninstantiated value for key', [true(BLookup == 2)]) :-
    openmap_key_value(O, a, 1),
    openmap_key_value(O, b, BUnknown),
    openmap_key_value(O, c, 3),

    openmap_key_value(O, a, A),
    openmap_key_value(O, b, BLookup),
    openmap_key_value(O, c, C),
    A == 1,
    var(BUnknown), var(BLookup),
    BUnknown = 2,
    C == 3.

test('openmap_key_value : value is ground + longer om', [true(
    openmap_key_value([a-1, b-2, c-3|_O], d, 100)
)]) :- true.

test('openmap_definite_key_value : fresh om', [true(
    \+ openmap_definite_key_value(_O, a, 1)
)]) :- true.

test('openmap_definite_key_value : singleton om', [true(
    openmap_definite_key_value([a-1|_O], a, 1)
)]) :- true.

test('openmap_definite_key_value : value is var', [true(A == 1)]) :-
    openmap_definite_key_value([a-1|_O], a, A).

test('openmap_definite_key_value : value is var + longer om + fail', [true(
    \+ openmap_definite_key_value([a-1, b-2, c-3|_O], d, _D)
)]) :- true.

test('openmap_definite_key_value : value is ground + longer om + fail', [true(
    \+ openmap_definite_key_value([a-1, b-2, c-3|_O], d, 100)
)]) :- true.

test('openmap_definite_key_value : value is var + longer om + succeed', [true(B == 2)]) :-
    openmap_definite_key_value([a-1, b-2, c-3|_O], b, B).

:- end_tests(openmap).