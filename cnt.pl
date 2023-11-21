:- module(cnt, [ns/2, load_syllables/1, analyse_lines/1, read_input/1]).
:- use_module(readLine,[readLine/1]).

ns([X|XS], N) :- map(ns, [X|XS], Sylls), sum(Sylls, N), !.
ns(X, N) :- \+ X = [], \+ X = [_, _], syl_db(X, N), !.
ns(X, N) :- \+ X = [], \+ X = [_, _], syl(X, N).

load_syllables(Fname) :-
    csv_read_file(Fname, Rows, [functor(row)]),
    assert_syllables(Rows).

analyse_lines(Ret) :-
    read_input(Input),
    map(analyse_line, Input, Ret).

vowel(a).
vowel(e).
vowel(i).
vowel(o).
vowel(u).
vowel(y).

split_list(_, [], Acc, Accs, Ret) :- reverse(Acc, ReverseAcc), reverse([ReverseAcc|Accs], Ret).
split_list(X, [X|Xs], Acc, Accs, Ret) :-
    reverse(Acc, ReverseAcc),
    split_list(X, Xs, [], [ReverseAcc | Accs], Ret), !.
split_list(X, [Y|Xs], Acc, Accs, Ret) :-
    \+ X = Y,
    split_list(X, Xs, [Y|Acc], Accs, Ret).

split_list(X, Xs, Ret) :- split_list(X, Xs, [], [], Ret).

num_syllables1([], _, 0).
num_syllables1([l, e], Prev, 1) :- \+ vowel(Prev).
num_syllables1([e], _, 0).
num_syllables1([X|XS], Prev, Ret) :- 
    \+ vowel(Prev), vowel(X),
    num_syllables1(XS, X, Num),
    Ret is Num + 1.

num_syllables1([X|XS], Prev, Num) :- (\+ vowel(X); vowel(Prev)), num_syllables1(XS, X, Num).

num_syllables([], 0).
num_syllables([X|XS], Ret) :- vowel(X), num_syllables1(XS, X, Num), Ret is Num + 1.
num_syllables([X|XS], Num) :- \+ vowel(X), num_syllables1(XS, X, Num).
num_syllables([_], 1).


syl(X, N) :- atom_chars(X, Chars), num_syllables(Chars, N).


:- use_module(library(csv)).

assert_syllables([]).
assert_syllables([row(Word, N) | Rows]) :- assert(syl_db(Word, N)), assert_syllables(Rows).

read_input1(Stream, []) :- at_end_of_stream(Stream).
read_input1(Stream, [X|XS]) :- \+ at_end_of_stream(Stream), read(Stream, X), read_input1(Stream, XS).

read_input(Fname, Ret) :-
    open(Fname, read, Str),
    readWords(Str, [], Ret),
    close(Str).
read_input(Ret) :- read_input('in.txt', Ret).

readWord(InStream, W, State):-
        get_code(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream, State),
        convertWord(Chars, Clean),
        atom_codes(W,Clean).
        
readWords(InStream, Acc, Ret) :-
    readWord(InStream, Word, State),
    !,
    continue(InStream, State, Word, Acc, Ret).

readWords(InStream, Acc, Ret) :-
    readWord(InStream, Word, State), !,
    State = newword,
    readWords(InStream, [Word | Acc], Ret).

continue(_, fin, Word, Acc, [Line]) :-
    reverse([Word | Acc], Line).
continue(InStream, newline, Word, Acc, [Line | Ret]) :-
    reverse([Word | Acc], Line),
    readWords(InStream, [], Ret).
continue(InStream, newword, Word, Acc, Ret) :-
    readWords(InStream, [Word | Acc], Ret).


convertWord([],[]):- !.

convertWord([Capital|Rest1],[Small|Rest2]):-
   Capital > 64, Capital < 91, !,
   Small is Capital + 32,
   convertWord(Rest1,Rest2).

convertWord([Number|Rest1],[Number|Rest2]):-
   Number > 47, Number < 58, !,
   convertWord(Rest1,Rest2).

convertWord([Weird|Rest1],Rest2):-
   (Weird < 97; Weird > 122), (\+ Weird = 10), !,
   convertWord(Rest1,Rest2).

convertWord([Char|Rest1],[Char|Rest2]):-
   convertWord(Rest1,Rest2).


checkCharAndReadRest(10,[],_, newline):- !.

checkCharAndReadRest(32,[],_, newword):- !.

checkCharAndReadRest(-1,[],_, fin):- !.

checkCharAndReadRest(end_of_file,[],_, fin):-  !.

% checkCharAndReadRest(Char,[Char|Chars],InStream, newline):-
%         Char = 10,
%         write('NOW THERES A NEWLINE'), !.


checkCharAndReadRest(Char,[Char|Chars],InStream, State):-
        get_code(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream, State).

add_tuple(pair(X1, X2), pair(Y1, Y2), pair(Z1, Z2)) :- Z1 is X1 + Y1, Z2 is X2 + Y2.

sum_tuples([], pair(0, 0)).
sum_tuples([X|XS], Ret) :- sum_tuples(XS, Sum), add_tuple(X, Sum, Ret).

sum([], 0).
sum([X|XS], Ret) :- sum(XS, Sum), Ret is X + Sum.

analyse_line1([], []).
analyse_line1([Word | Sentence], [N_syl | Ret]) :- ns(Word, N_syl), analyse_line1(Sentence, Ret).
analyse_line(Sentence, pair(SumRet, LenRet)) :-
    analyse_line1(Sentence, Ret),
    length(Ret, LenRet), sum(Ret, SumRet).


analyse_line(Ret) :- readLine(Input), analyse_line(Input, Ret).

map(_, [], []).
map(Pred, [X|Xs], [Y|Ys]) :- call(Pred, X, Y), map(Pred, Xs, Ys).
