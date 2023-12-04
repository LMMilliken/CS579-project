:- module(answerQuestion, [answerQuestion/2]).

:- use_module(readLine,[readLine/1]).
:- use_module(modelChecker2,[satisfy/4]).
:- use_module(lambda, [lambda/2]).
:- use_module(classify, [classify/1, extract_attributes/1]).
:- use_module(model, [create_model/0, create_model/1, create_model/2, query/2]).
:- use_module(cnt, [load_syllables/0]).
:- use_module(comsemPredicates,[printRepresentations/1]).

:- initialization(load_syllables).

/*========================================================================
   Driver For Project
========================================================================*/

analyzer:-
	readLine(Question),
    lambda(Question, FOLQuestion),
    create_model(Model), !,
    %If we want to print the FOL representation
    printRepresentations(FOLQuestion),
    answerQuestion(FOLQuestion, Model).

/*========================================================================
    Answer Questions
========================================================================*/

answerQuestion(que(X,R,S),Model):-
writeln(and(R,S)),
    satisfy(and(R,S),Model,Value,Result),
    \+ Result=undef,
    writeln(Value).
