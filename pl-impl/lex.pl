:- module(lex, [
    tokens//1,
    op(12, xfy, @)
]).

:- use_module(library(dcg/basics), [
    % csym//1, % WHY NO WORK??
    integer//1,
    string_without//2,
    string//1,
    eos//0
]).

:- op(12, xfy, @).

:- set_prolog_flag(double_quotes, chars).
:- current_prolog_flag(dialect, swi).
:- current_prolog_flag(version_data, swi(9, _Minor, _Patch, _Extra)).


tokens(Ts) --> tokens(Ts, 1).

tokens([T @ Line | Ts], Line0) -->
    ws(NewLines),
    tok(T),
    !,
    { Line is Line0 + NewLines },
    tokens(Ts, Line).
tokens([], _NLines) --> ws(_), eos.


ws(NLs) --> ws_(0, NLs).

ws_(NLs0, NLs) --> ([' '] | ['\t']), !, ws_(NLs0, NLs).
ws_(NLs0, NLs) --> (['\n'] | ['\r', '\n']), !, { NLs1 is NLs0 + 1 }, ws_(NLs1, NLs).
ws_(NLs,  NLs) --> [].

tok( lit(nat(I)) ) --> integer(I), !.
tok( lit(int(I)) ) --> (['-'] | ['+']), integer(I), !.
tok( lit(bool(true)) ) --> atom(true), !.
tok( lit(bool(false)) ) --> atom(false), !.
tok( lit(text(Txt)) ) --> ['"'], !, string_without("\"\n", Txt), ['"'].
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