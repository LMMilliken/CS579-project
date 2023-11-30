:- module(cnt, [ns/2, load_syllables/1, load_syllables/0, analyse_lines/1, analyse_lines/2, read_input/1]).
:- use_module(utils, [add_tuple/3, sum_tuples/2, sum/2]).
:- use_module(read, [read_input/1]).
:- use_module(library(csv)).

ns([X|XS], N) :- map(ns, [X|XS], Sylls), sum(Sylls, N), !.
ns(X, N) :- \+ X = [], \+ X = [_, _], syl_db(X, N), !.
ns(X, N) :- \+ X = [], \+ X = [_, _], syl(X, N).

load_syllables(Fname) :-
    csv_read_file(Fname, Rows, [functor(row)]),
    assert_syllables(Rows).

load_syllables :- 
    csv_read_file('data.csv', Rows, [functor(row)]),
    assert_syllables(Rows).


map(_, [], []).
map(Pred, [X|Xs], [Y|Ys]) :- call(Pred, X, Y), map(Pred, Xs, Ys).

analyse_lines(Input, Ret) :- 
    map(analyse_line, Input, Ret).

analyse_lines(Ret) :-
    read_input(Input),
    analyse_lines(Input, Ret).

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



assert_syllables([]).
assert_syllables([row(Word, N) | Rows]) :- assert(syl_db(Word, N)), assert_syllables(Rows).


analyse_line1([], []).
analyse_line1([Word | Sentence], [N_syl | Ret]) :- ns(Word, N_syl), analyse_line1(Sentence, Ret).
analyse_line(Sentence, tuple(SumRet, LenRet)) :-
    analyse_line1(Sentence, Ret),
    length(Ret, LenRet), sum(Ret, SumRet).