#!/usr/bin/env swipl

:- module(main, []).
:- use_module(main_, [main_/1]).
:- use_module(library(main)).

:- initialization(main).

main(ArgV) :- main_(ArgV).
