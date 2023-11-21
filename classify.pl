:- use_module(cnt, [load_syllables/1, analyse_lines/1, read_input/1]).
:- use_module(profile, [haiku/2]).

classify(pair(Styles, Authors)) :-
    analyse_lines(Input),
    classify_styles(Input, Styles),
    classify_authors(Styles, Authors).

classify_styles(Input, Classes) :-
    findall(Class, classify_style(Input, Class), Classes).

classify_style([pair(5, _), pair(7, _), pair(5, _)], haiku).
classify_style(Input, syls(N)) :- list_all(Input, pair(N, _)).
classify_style(Input, lines(N)) :- length(Input, N).
% classify_style(_, undefined).

classify_work(Styles, the_raven) :- subset([syls(18), lines(6)], Styles).

classify_authors(Styles, Authors) :-
    findall(Author, classify_author(Styles, Author), Authors).
classify_author(Styles, shakespeare) :- subset([iambic_pentameter, lines_14], Styles).


is_10(pair(10, _)).

num_lines([], 0).
num_lines([_|XS], N) :- num_lines(XS, M), N is M + 1.

list_all([], _).
list_all([X|XS], Pattern) :- X = Pattern, list_all(XS, Pattern).