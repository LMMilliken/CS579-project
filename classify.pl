:- use_module(cnt, [load_syllables/1, analyse_lines/1, analyse_lines/2, read_input/1]).
:- use_module(profile, [haiku/2]).
:- use_module(rhymesChecker, [understand_structure/3]).
:- use_module(utils, [map/3, end/2, lowest_precision/2]).

classify(tuple(Structures, Styles, Rhymes, Authors)) :-
    read_input(Input),
    analyse_lines(Input, Analysis),
    classify_structures(Analysis, Structures),
    classify_styles(Analysis, Structures, Styles),
    classify_rhymes(Input, Rhymes),
    classify_authors(Structures, Styles, Rhymes, Authors).

classify_structures(Input, Structures) :- findall(Structure, classify_structure(Input, Structure), Structures).

classify_structure(Input, syls(N, exact)) :- list_all(Input, N).
classify_structure(Input, syls(N, approx)) :- list_all_approx(Input, N, 1, _).
% classify_structure(Input, syls(N, approx(NumMissed))) :- list_all_approx(Input, N, 1, NumMissed).
classify_structure(Input, lines(N)) :- length(Input, N).


classify_styles(Analysis, Structure, Classes) :-
    findall(Class, classify_style(Analysis, Structure, Class), Classes).

classify_style([tuple(5, _), tuple(7, _), tuple(5, _)], _, tuple(haiku, exact)).

classify_rhymes(Input, Rhymes) :-
    map(end, Input, Ends),
    findall(tuple(Rhyme, Precision), understand_structure(Ends, Rhyme, Precision), Rhymes).


classify_work(Styles, the_raven) :- subset([syls(18), lines(6)], Styles).

classify_authors(Structures, Styles, Rhymes, Authors) :-
    findall(Author, classify_author(Structures, Styles, Rhymes, Author), Authors).

classify_author(Structures, _, Rhymes, tuple(shakespeare, Precision)) :- 
    member(syls(10, P1), Structures), member(lines(14), Structures),
    member(tuple(english_sonnet, P2), Rhymes), lowest_precision([P1, P2], Precision).


num_lines([], 0).
num_lines([_|XS], N) :- num_lines(XS, M), N is M + 1.

list_all([], _).
list_all([tuple(X, _)|XS], Pattern) :- X = Pattern, list_all(XS, Pattern).

list_all_approx([], _, _, 0) :- !.
list_all_approx([tuple(X, _)|XS], N, Margin, Ret) :- X = N, list_all_approx(XS, N, Margin, Ret), !.
list_all_approx([tuple(X, _)|XS], N, Margin, Ret) :-
    \+ X = N,
    X >= N - Margin, X < N + Margin + 1,
    list_all_approx(XS, N, Margin, Missed), Ret is Missed + 1.