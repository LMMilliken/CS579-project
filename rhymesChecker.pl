:- module(rhymesChecker, [understand_structure/3]).
:- use_module(lexicon, [ph/2]).
:- use_module(utils, [last_phon/2, lowest_precision/2]).
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
are_in_rhyme(X, Y, exact):- ph(X, XLEX), ph(Y, YLEX), reverse(XLEX, REVX), reverse(YLEX, REVY),
    last_elements(REVX, A), last_elements(REVY, B), A == B, !.
% If one word is not in the lexicon we use it as jolly, so it rhymes with every other word (to be able to handle unknown words)
% Decide if we want this behavior
are_in_rhyme(X, Y, approx) :- approximate_rhyme(X, Y), !.
% are_in_rhyme(X, Y, exact) :-
%     (
%         \+ ph(X, _), ph(Y, _);
%         ph(X, _), \+ ph(Y, _)
%     ),
%     approximate_rhyme(X, Y), !.


approximate_rhyme(X, Y) :- last_phon(X, XPHON), last_phon(Y, YPHON), XPHON = YPHON.

check_quartine_single(X, Y, W, Z, Precision) :- are_in_rhyme(X, Y, P1), are_in_rhyme(Y, W, P2), are_in_rhyme(W, Z, P3),
    lowest_precision([P1, P2, P3], Precision).

%AAAA AAAA AAAA
check_mono([X, Y, W, Z, A, B, C, D], Precision) :- check_quartine_single(X, Y, W, Z, P1), check_quartine_single(A, B, C, D, P2), are_in_rhyme(Z, A, P3),
    lowest_precision([P1, P2, P3], Precision).

%AAAA BBBB CCCC 
check_quartine([X, Y, W, Z, A, B, C, D], Precision) :- check_quartine_single(X, Y, W, Z, P1), check_quartine_single(A, B, C, D, P2),
    lowest_precision([P1, P2], Precision).

check_kissed_single(X, Y, W, Z, Precision) :- are_in_rhyme(X, Y, P1), are_in_rhyme(W, Z, P2),
    lowest_precision([P1, P2], Precision).

% Example of this: The Canterbury Tales, Chaucer
%AABB CCDD EEFF
check_kissed([X, Y, W, Z, A, B, C, D], Precision) :- check_kissed_single(X, Y, W, Z, P1), check_kissed_single(A, B, C, D, P2),
    lowest_precision([P1, P2], Precision).

check_concatenated_single(X, Y, W, Z, Precision) :- are_in_rhyme(X, W, P1), are_in_rhyme(Y, Z, P2),
    lowest_precision([P1, P2], Precision).

%ABAB CDCD EFEF
check_concatenated([X, Y, W, Z, A, B, C, D], Precision) :- check_concatenated_single(X, Y, W, Z, P1), check_concatenated_single(A, B, C, D, P2),
    lowest_precision([P1, P2], Precision).

check_closed_single(X, Y, W, Z, Precision) :- are_in_rhyme(X, Z, P1), are_in_rhyme(Y, W, P2),
    lowest_precision([P1, P2], Precision).

%ABBA CDDC EFFE
check_closed([X, Y, W, Z, A, B, C, D], Precision) :- check_closed_single(X, Y, W, Z, P1), check_closed_single(A, B, C, D, P2),
    lowest_precision([P1, P2], Precision).

check_incatenated_single(X, Y, W, P1) :- are_in_rhyme(X, W, P1), \+ are_in_rhyme(Y, W, exact).

%ABA BCB CDC DED
check_incatenated([X, Y, W, Z, A, B, C, D], Precision) :- check_incatenated_single(X, Y, W, P1), check_incatenated_single(Z, A, B, P2),
    are_in_rhyme(Y, Z, P3), are_in_rhyme(A, C, P4), \+ are_in_rhyme(C, D, exact),
    lowest_precision([P1, P2, P3, P4], Precision).

%ABAB CDCD EFEF GG
check_english_sonnet([V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14], Precision) :- check_concatenated([V1, V2, V3, V4, V5, V6, V7, V8], P1), check_concatenated_single(V9, V10, V11, V12, P2), are_in_rhyme(V13, V14, P3),
    \+ are_in_rhyme(V1, V5, exact), \+ are_in_rhyme(V1, V6, exact), \+ are_in_rhyme(V1, V9, exact), \+ are_in_rhyme(V1, V10, exact), \+ are_in_rhyme(V1, V13, exact),
    \+ are_in_rhyme(V2, V5, exact), \+ are_in_rhyme(V2, V6, exact), \+ are_in_rhyme(V2, V9, exact), \+ are_in_rhyme(V2, V10, exact), \+ are_in_rhyme(V2, V13,exact),
    \+ are_in_rhyme(V5, V9, exact), \+ are_in_rhyme(V5, V10, exact), \+ are_in_rhyme(V5, V13, exact),
    \+ are_in_rhyme(V6, V9, exact), \+ are_in_rhyme(V6, V10, exact), \+ are_in_rhyme(V6, V13, exact),
    \+ are_in_rhyme(V9, V13, exact),
    \+ are_in_rhyme(V10, V13, exact),
    lowest_precision([P1, P2, P3], Precision).

%ABA ABA ABA
check_villenelle([X, Y, Z, A, B, C], Precision) :- check_incatenated_single(X, Y, Z, P1), check_incatenated_single(A, B, C, P2), are_in_rhyme(X, A, P3), are_in_rhyme(B, Y, P4),
    lowest_precision([P1, P2, P3, P4], Precision).

understand_structure([V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14], english_sonnet, Precision) :- 
    check_english_sonnet([V1, V2, V3, V4, V5, V6, V7, V8, V9, V10, V11, V12, V13, V14], Precision), !.
understand_structure([X, Y, W, Z, A, B, C, D | _], mono, Precision) :- check_mono([X, Y, W, Z, A, B, C, D], Precision), !.
understand_structure([X, Y, W, Z, A, B, C, D | _], quartine, Precision) :- check_quartine([X, Y, W, Z, A, B, C, D], Precision), !.
understand_structure([X, Y, W, Z, A, B, C, D | _], kissed, Precision) :- check_kissed([X, Y, W, Z, A, B, C, D], Precision), !.
understand_structure([X, Y, W, Z, A, B, C, D | _], concatenated, Precision) :- check_concatenated([X, Y, W, Z, A, B, C, D], Precision), !.
understand_structure([X, Y, W, Z, A, B, C, D | _], closed, Precision) :- check_closed([X, Y, W, Z, A, B, C, D], Precision), !.
understand_structure([X, Y, W, Z, A, B, C, D | _], incatenated, Precision) :- check_incatenated([X, Y, W, Z, A, B, C, D], Precision), !.
understand_structure([X, Y, W, Z, A, B |_], villenelle, Precision) :- check_villenelle([X, Y, W, Z, A, B], Precision), !.

% understand_structure(_, unknown).