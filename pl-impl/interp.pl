
and(true, true, true) :- !.
and(_, _, false).

or(true, _, true) :- !.
or(_, true, true) :- !.
or(_, _, false).

not(true, false).
not(false, true).


exe([over]), [B, A, B] --> [A, B].
exe([rot]), [C, A, B] --> [A, B, C].

exe([and]), [C] --> [A, B], { and(A, B, C) }.
exe([or]), [C] --> [A, B], { or(A, B, C) }.
exe([not]), [B] --> [A], { not(A, B) }.

exe([const(word), _]).