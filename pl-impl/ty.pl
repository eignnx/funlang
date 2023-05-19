:- module(ty, [type/1, type_size/2]).

type(void).
type(bool).
type(nat).
type(int).

%% Size in bytes.
type_size(void, 0).
type_size(int, 8). % TODO: Make arch independent?
type_size(nat, 8). % TODO: Make arch independent?
type_size(bool, 1).