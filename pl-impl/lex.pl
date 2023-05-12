:- module(lex, [tokens//1]).

:- use_module(library(dcg/basics), [
    % csym//1, % WHY NO WORK??
    integer//1,
    string_without//2,
    string//1,
    blanks//0,
    eos//0
]).

:- set_prolog_flag(double_quotes, chars).
:- current_prolog_flag(dialect, swi).
:- current_prolog_flag(version_data, swi(9, _Minor, _Patch, _Extra)).


tokens([T | Ts]) --> blanks, tok(T), !, tokens(Ts).
tokens([]) --> blanks, eos.

tok( lit(nat(I)) ) --> integer(I), !.
tok( lit(int(I)) ) --> (['-'] | ['+']), integer(I), !.
tok( lit(bool(true)) ) --> atom(true), !.
tok( lit(bool(false)) ) --> atom(false), !.
tok( lit(text(Txt)) ) --> ['"'], !, string_without("\"", Txt), ['"'].
tok( kw(Kw) ) --> { keyword(Kw) }, atom(Kw), !.
tok( id(Id) ) --> csym(Id).
tok( sym(Sym) ) --> { symbol(Sym) }, atom(Sym), !.

keyword(def).
keyword(type). keyword(rec).
keyword(do). keyword(end).
keyword(if). keyword(match). keyword(while).
keyword(fn).
keyword(let).
keyword(and). keyword(or). keyword(not). keyword(xor).

symbol('('). symbol(')').
symbol('['). symbol(']').
symbol('{'). symbol('}').
symbol('...'). symbol('..='). symbol('..'). symbol('.').
symbol(',').
symbol('::').
symbol(':=').
symbol(':').
symbol(';').
symbol('->').
symbol('=>').
symbol('==').
symbol('!=').
symbol('=').
symbol('!!'). symbol('||').
symbol('|').
symbol('!'). symbol('~').
symbol('++'). symbol('**'). symbol('^').
symbol('+'). symbol('-'). symbol('*'). symbol('/').
symbol('<<'). symbol('>>').
symbol('>='). symbol('<='). symbol('>'). symbol('<').

% COPY PASTED FROM SWI SOURCE.
csym(Name) -->
    [F], {code_type(F, csymf)},
    csyms(Rest),
    { atom_codes(Name, [F|Rest]) }.

csyms([H|T]) -->
    [H], {code_type(H, csym)},
    !,
    csyms(T).
csyms([]) -->
    "".

atom(A) --> { atom_chars(A, Cs) }, string(Cs).