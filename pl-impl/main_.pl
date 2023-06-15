:- module(main_, [main_/1]).

:- use_module(lex, [tokens//1]).
:- use_module(parse, [item//1]).
:- use_module(tycheck, [tycheck/2]).
:- use_module(tast_to_hir, [hir//1]).
:- use_module(hir_to_lir, [hir_to_lir//1]).

:- use_module(library(pio), [phrase_from_file/2]).
:- use_module(library(listing), [portray_clause/1]).


main_([SrcFile]) :-
    !,
    catch(
        (
            tokens_from_file(Tokens, SrcFile),
            format('Tokens = '), portray_clause(Tokens),
            phrase(item(Item), Tokens), % Parse an `item` from the token stream.
            format('Ast = '), portray_clause(Item),
            (
                tycheck(Item, Tast) -> % Do typechecking to produce a typed AST.
                    format('Tast = '), portray_clause(Tast)
                ;
                    Item = @(_, Ln),
                    throw(error('Typechecking failed!', Ln))
            ),
            phrase(item_hir(Tast), Hir), % Lower the TAST to HIR.
            format('Hir = '), portray_clause(Hir),
            phrase(hir_to_lir(Hir), Lir), % Lower HIR to LIR.
            format('Lir = '), portray_clause(Lir)
        ),
        error(ty_err(Err), @(Description, Line)),
        (
            ansi_format([bold, fg(red)], 'Error[~a:~w]:~n', [SrcFile, Line]),
            ansi_format([fg(white)], '~4|~t~w, ~w~n', [Err, Description])
        )
    ).

main_(_) :-
    throw(error('Please provide a source file.')).


tokens_from_file(Tokens, SrcFile) :-
    phrase_from_file(seq(Codes), SrcFile),
    % For some reason, `phrase_from_file` operates with codes not chars.
    maplist(char_code, Chars, Codes),
    phrase(tokens(Tokens), Chars).

% FROM: https://github.com/mthom/scryer-prolog/blob/462097d95615181d45d20cfed4daac28d0a6a815/src/lib/dcgs.pl#L176-L186
%% seq(Seq)//
% 
% Describes a sequence
seq(Xs, Cs0,Cs) :-
   var(Xs),
   Cs0 == [],
   !,
   Xs = [],
   Cs0 = Cs.
seq([]) --> [].
seq([E|Es]) --> [E], seq(Es).