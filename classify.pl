:- use_module(cnt, [load_syllables/1, analyse_lines/1, analyse_lines/2, read_input/1]).
:- use_module(profile, [haiku/2]).
:- use_module(rhymesChecker, [understand_structure/3]).
:- use_module(utils, [map/3, end/2, max_tuples/2, lowest_precision/2, format_tuples/2]).

find_n_lines(NLines) :-
    analyse_lines(Input),
    classify_structure(Input, lines(NLines)), !.

find_rhyme_scheme(RhymeSchemes) :-
    read_input(Input),
    map(end, Input, Ends),
    findall((Rhyme, Precision), understand_structure(Ends, Rhyme, Precision), RhymeSchemes), !.

find_most_likely_author(MaxAuthorsFormatted) :- 
    classify(R), !,
    R = tuple(_, _,_, Authors),
    max_tuples(Authors, MaxAuthors),!,
    format_tuples(MaxAuthors, MaxAuthorsFormatted).

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
    findall(tuple(Author, Matching_properties), classify_author(Structures, Styles, Rhymes, Author, Matching_properties), Authors), !.

classify_author(Structure,_,Rhymes, tuple(shakespeare, Precision), Matching_properties) :- 
    check_property(lines(14), Structure, 0, N1), 
    check_property(syls(10, P1), Structure, N1, N2), 
    check_property(tuple(english_sonet, P2), Rhymes, N2, N3), 
    lowest_precision([P1, P2], Precision), 
    Matching_properties = N3.
    
classify_author(Structure,_,Rhymes, tuple(elizabeth_bishop, Precision), Matching_properties) :- 
    check_property(lines(19), Structure, 0, N1),
    check_property(syls(10, P1), Structure, N1, N2),
    check_property(tuple(villenelle, P2), Rhymes, N2, N3),
    lowest_precision([P1, P2], Precision),
    Matching_properties = N3.

check_property(Target, Properties, N, NewN) :- (subset([Target], Properties), !, NewN is N + 1; NewN is N).

is_10(tuple(10, _)).

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