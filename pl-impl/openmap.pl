:- module(openmap, [openmap_key_value/3]).

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
% If a key is mapped to a different value, an uninstantiation error
% is thrown. This is probably due to a flaw in your program's
% logic (no offense).
openmap_key_value([K1-V1 | M], K2, V2) :-
    ground(K2) ->
        openmap_key_value_([K1-V1 | M], K2, V2)
    ;
        instantiation_error(K2).

openmap_key_value_([K-V1 | _], K, V2) :- !,
    ( V1 = V2 -> true ; uninstantiation_error(V2) ).
openmap_key_value_([_-_ | M], K, V) :-
    openmap_key_value_(M, K, V).
