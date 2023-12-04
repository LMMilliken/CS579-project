:- module(answerQuestion, [answerQuestion/2]).

:- use_module(readLine,[readLine/1]).
:- use_module(modelChecker2,[satisfy/4]).
:- use_module(lambda, [lambda/2]).
:- use_module(classify, [classify/1, extract_attributes/1, find_most_likely_author/1]).
:- use_module(model, [create_model/0, create_model/1, create_model/2, query/2]).
:- use_module(cnt, [load_syllables/0]).
:- use_module(comsemPredicates,[printRepresentations/1]).

:- initialization(load_syllables).

/*========================================================================
   Driver For Project
========================================================================*/

analyzer:-
	readLine(Question),
    lambda(Question, [FOLQuestion]),
    create_model(Model), !,
    printRepresentations([FOLQuestion]),
    writeln("Model: "),
    writeln(Model),
    answerQuestion(FOLQuestion, Model).

analyzer2:-
	readLine(Question),
    lambda(Question, [_]),
    find_most_likely_author(Authors),
    write_results_authors(Authors).

/*========================================================================
    Answer Questions
========================================================================*/

answerQuestion(que(_,R,S),Model):-
    satisfy(and(R,S),Model,Value,Result),
    \+ Result=undef,
    writeln(Value).


/*========================================================================
    Format the authors result from the prolog classifier
========================================================================*/

write_results_authors([]).
write_results_authors([(Author, approx,_)]):-
    !,
    write("We had to do some approximation, but we found that the possible author is: "),
    write(Author),
    write("."),
    nl.
write_results_authors([(Author, exact,_)]):-
    !,
    write("We found that the possible author is: "),
    write(Author),
    write("."),
    nl.
write_results_authors([(Author, approx,_)|Authors]):-
    !,
    write("We had to do some approximation, but we found that the possible authors are: "),
    write(Author),
    write_author_list(Authors),
    nl.
write_results_authors([(Author, exact,_)|Authors]):-
    !,
    write("We found that the possible authors are: "),
    write(Author),
    write_author_list(Authors),
    nl.

write_author_list([]).
write_author_list([(Author, approx,_) | Authors]):-
    !,
    write(" and "),
    write(Author),
    write_author_list(Authors).