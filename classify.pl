:- use_module(cnt, [load_syllables/1, analyse_lines/1, analyse_lines/2, read_input/1]).
:- use_module(profile, [haiku/2]).
:- use_module(rhymesChecker, [understand_structure/2]).
:- use_module(utils, [map/3, end/2]).

classify(tuple(Styles, Rhymes, Authors)) :-
    read_input(Input),
    analyse_lines(Input, Analysis),
    classify_styles(Analysis, Styles),
    classify_rhymes(Input, Rhymes),
    classify_authors(Styles, Authors).

classify_styles(Analysis, Classes) :-
    findall(Class, classify_style(Analysis, Class), Classes).

classify_style([tuple(5, _), tuple(7, _), tuple(5, _)], haiku).
classify_style(Input, syls(N)) :- list_all(Input, tuple(N, _)).
classify_style(Input, lines(N)) :- length(Input, N).

classify_rhymes(Input, Rhymes) :-
    map(end, Input, Ends),
    findall(Rhyme, understand_structure(Ends, Rhyme), Rhymes).


classify_work(Styles, the_raven) :- subset([syls(18), lines(6)], Styles).

classify_authors(Styles, Authors) :-
    findall(Author, classify_author(Styles, Author), Authors).

classify_author(Styles, shakespeare) :- subset([iambic_pentameter, lines_14], Styles).


is_10(tuple(10, _)).

num_lines([], 0).
num_lines([_|XS], N) :- num_lines(XS, M), N is M + 1.

list_all([], _).
list_all([X|XS], Pattern) :- X = Pattern, list_all(XS, Pattern).

list_all_approx([], _, _, 0) :- !.
list_all_approx([X|XS], Pattern, Margin, Ret) :- X = Pattern, list_all_approx(XS, Pattern, Margin, Ret).
list_all_approx([X|XS], Pattern, Margin, Ret) :-
    \+ X = Pattern,
    X >= Pattern - Margin, X < Pattern + Margin + 1,
    list_all_approx(XS, Pattern, Margin, Missed), Ret is Missed + 1.