:- use_module(cnt, [load_syllables/1, analyse_lines/1, analyse_lines/2, read_input/1]).
:- use_module(profile, [haiku/2]).
:- use_module(rhymesChecker, [understand_structure/2]).
:- use_module(utils, [map/3, end/2, max_tuples/2]).

find_n_lines(NLines) :-
    analyse_lines(Input),
    classify_style(Input, NLines), !.

find_rhyme_scheme(RhymeSchemes) :-
    read_input(Input),
    map(end, Input, Ends),
    findall(Rhyme, understand_structure(Ends, Rhyme), RhymeSchemes), !.

find_most_likely_author(MaxAuthors) :- 
    classify(R), !,
    R = tuple(_, _, Authors),
    max_tuples(Authors, MaxAuthors),!.

classify(tuple(Styles, Rhymes, Authors)) :-
    read_input(Input),
    analyse_lines(Input, Analysis),
    classify_styles(Analysis, Styles),
    classify_rhymes(Input, Rhymes),
    classify_authors(Rhymes, Styles, Authors).

classify_styles(Analysis, Classes) :-
    findall(Class, classify_style(Analysis, Class), Classes).

classify_style([tuple(5, _), tuple(7, _), tuple(5, _)], haiku).
classify_style(Input, syls(N)) :- list_all(Input, tuple(N, _)).
classify_style(Input, lines(N)) :- length(Input, N).

classify_rhymes(Input, Rhymes) :-
    map(end, Input, Ends),
    findall(Rhyme, understand_structure(Ends, Rhyme), Rhymes).

classify_work(Styles, the_raven) :- subset([syls(18), lines(6)], Styles).

classify_authors(Rhymes, Styles, Authors) :-
    findall(tuple(Author, Matching_properties), classify_author(Rhymes, Styles, Author, Matching_properties), Authors).

classify_author(Rhymes, Styles, shakespeare, Matching_properties) :- 
    check_property(lines(14), Styles, 0, N1),
    check_property(iambic_pentameter, Styles, N1, N2),
    check_property(english_sonet, Rhymes, N2, N3),
    Matching_properties = N3.
classify_author(Rhymes, Styles, elizabeth_bishop, Matching_properties) :-
    check_property(lines(19), Styles, 0, N1),
    check_property(iambic_pentameter, Styles, N1, N2),
    check_property(villenelle, Rhymes, N2, N3),
    Matching_properties = N3.

check_property(Target, Properties, N, NewN) :- (subset([Target], Properties), !, NewN is N + 1; NewN is N).

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