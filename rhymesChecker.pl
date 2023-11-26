:- module(rhymesChecker, [understand_structure/2]).
:- use_module(lexicon, [ph/2]).
:- use_module(utils, [last_phon/2]).
% Define a predicate to check if a character is a vowel
% I check only for upper cases, since the dataset is built using those
is_vowel(Char) :-!,
    member(Char, ['A', 'E', 'I', 'O', 'U']),!.

% Reverse function
reverse(List, Reversed) :-
    reverse_acc(List, [], Reversed).

% Reverse with accumulator
reverse_acc([], Acc, Acc).
reverse_acc([Head|Tail], Acc, Reversed) :-
    reverse_acc(Tail, [Head|Acc], Reversed).

% Define a predicate to check if a string contains a vowel
contains_vowel(String) :-!,
    atom_chars(String, Chars),
    member(Vowel, Chars),
    is_vowel(Vowel).

% 2 words rhyme if they have the same phonetic starting from the last vowel-ish sound to the end

% Get the last elements to check for rhyme
last_elements([], []).
last_elements([Head|_], Prefix) :-
    contains_vowel(Head), !,
    Prefix = [Head].
last_elements([Head|Tail], Prefix) :-
    \+ contains_vowel(Head),
    last_elements(Tail, TailPrefix),
    Prefix = [Head|TailPrefix].

% Given 2 words check if they are in rhyme
are_in_rhyme(X, Y):- ph(X, XLEX), ph(Y, YLEX),!, reverse(XLEX, REVX), reverse(YLEX, REVY),
    last_elements(REVX, A), last_elements(REVY, B), A == B, !.
% If one word is not in the lexicon we use it as jolly, so it rhymes with every other word (to be able to handle unknown words)
% Decide if we want this behavior
are_in_rhyme(X, Y) :-
    (
        \+ ph(X, _), ph(Y, _);
        ph(X, _), \+ ph(Y, _)
    ),
    approximate_rhyme(X, Y).

approximate_rhyme(X, Y) :- last_phon(X, XPHON), last_phon(Y, YPHON), XPHON = YPHON.


check_quartine_single(X, Y, W, Z) :- are_in_rhyme(X, Y), are_in_rhyme(Y, W), are_in_rhyme(W, Z).

%AAAA AAAA AAAA
check_mono([X, Y, W, Z, A, B, C, D]) :- check_quartine_single(X, Y, W, Z), check_quartine_single(A, B, C, D), are_in_rhyme(Z, A).

%AAAA BBBB CCCC 
check_quartine([X, Y, W, Z, A, B, C, D]) :- check_quartine_single(X, Y, W, Z), check_quartine_single(A, B, C, D).

check_kissed_single(X, Y, W, Z) :- are_in_rhyme(X, Y), are_in_rhyme(W, Z).

% Example of this: The Canterbury Tales, Chaucer
%AABB CCDD EEFF
check_kissed([X, Y, W, Z, A, B, C, D]) :- check_kissed_single(X, Y, W, Z), check_kissed_single(A, B, C, D).

check_concatenated_single(X, Y, W, Z) :- are_in_rhyme(X, W), are_in_rhyme(Y, Z).

%ABAB CDCD EFEF
check_concatenated([X, Y, W, Z, A, B, C, D]) :- check_concatenated_single(X, Y, W, Z), check_concatenated_single(A, B, C, D).

check_closed_single(X, Y, W, Z) :- are_in_rhyme(X, Z), are_in_rhyme(Y, W).

%ABBA CDDC EFFE
check_closed([X, Y, W, Z, A, B, C, D]) :- check_closed_single(X, Y, W, Z), check_closed_single(A, B, C, D).

check_incatenated_signle(X, Y, W) :- are_in_rhyme(X, W), \+ are_in_rhyme(Y, W).

%ABA BCB CDC DED
check_incatenated([X, Y, W, Z, A, B, C, D]) :- check_incatenated_signle(X, Y, W), check_incatenated_signle(Z, A, B), are_in_rhyme(Y, Z), are_in_rhyme(A, C), \+are_in_rhyme(C, D).

%ABAB CDCD EFEF GG
check_english_sonet(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14) :- check_concatenated(V1, V2, V3, V4, V5, V6, V7, V8), check_concatenated_single(V9, V10, V11, V12), are_in_rhyme(V13, V14),
    \+ are_in_rhyme(V1, V5), \+ are_in_rhyme(V1, V6), \+ are_in_rhyme(V1, V9), \+ are_in_rhyme(V1, V10), \+ are_in_rhyme(V1, V13),
    \+ are_in_rhyme(V2, V5), \+ are_in_rhyme(V2, V6), \+ are_in_rhyme(V2, V9), \+ are_in_rhyme(V2, V10), \+ are_in_rhyme(V2, V13),
    \+ are_in_rhyme(V5, V9), \+ are_in_rhyme(V5, V10), \+ are_in_rhyme(V5, V13),
    \+ are_in_rhyme(V6, V9), \+ are_in_rhyme(V6, V10), \+ are_in_rhyme(V6, V13),
    \+ are_in_rhyme(V9, V13),
    \+ are_in_rhyme(V10, V13).

understand_structure([X, Y, W, Z, A, B, C, D], mono) :- check_mono([X, Y, W, Z, A, B, C, D]), !.
understand_structure([X, Y, W, Z, A, B, C, D], quartine) :- check_quartine([X, Y, W, Z, A, B, C, D]), !.
understand_structure([X, Y, W, Z, A, B, C, D], kissed) :- check_kissed([X, Y, W, Z, A, B, C, D]), !.
understand_structure([X, Y, W, Z, A, B, C, D], concatenated) :- check_concatenated([X, Y, W, Z, A, B, C, D]), !.
understand_structure([X, Y, W, Z, A, B, C, D], closed) :- check_closed([X, Y, W, Z, A, B, C, D]), !.
understand_structure([X, Y, W, Z, A, B, C, D], incatenated) :- check_incatenated([X, Y, W, Z, A, B, C, D]), !.

% understand_structure(_, unknown).

% Authors
% Classifier
