:- module(utils, [map/3, add_tuple/3, sum_tuples/2, sum/2, end/2, last_phon/2]).

map(_, [], []).
map(Pred, [X|Xs], [Y|Ys]) :- call(Pred, X, Y), map(Pred, Xs, Ys).

add_tuple(tuple(X1, X2), tuple(Y1, Y2), tuple(Z1, Z2)) :- Z1 is X1 + Y1, Z2 is X2 + Y2.

sum_tuples([], tuple(0, 0)).
sum_tuples([X|XS], Ret) :- sum_tuples(XS, Sum), add_tuple(X, Sum, Ret).

sum([], 0).
sum([X|XS], Ret) :- sum(XS, Sum), Ret is X + Sum.

end([], []).
end([X|[]], X) :- !.
end([_|XS], Ret) :- end(XS, Ret).

vowel(X) :- member(X, ['a', 'e', 'i', 'o', 'u']).

% Define the main function
last_phon(Atom, Sublist) :-
    atom_chars(Atom, CharList),
    reverse(CharList, Reversed),
    extract_after_last_vowel(Reversed, Sublist).

% Define a helper predicate to extract the sublist after the last vowel
extract_after_last_vowel([], []).
extract_after_last_vowel([H|T], [H|Sublist]) :-
    \+ vowel(H),
    extract_after_last_vowel(T, Sublist).
extract_after_last_vowel([H|_], []) :-
    vowel(H).