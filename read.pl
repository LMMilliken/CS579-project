:- module(read, [read_input/1, read_input/2]).

read_input(Fname, Ret) :-
    open(Fname, read, Str),
    readWords(Str, [], Ret),
    close(Str).
read_input(Ret) :- read_input('in.txt', Ret).


readWords(InStream, Acc, Ret) :-
    readWord(InStream, Word, State),
    !,
    continue(InStream, State, Word, Acc, Ret).

readWords(InStream, Acc, Ret) :-
    readWord(InStream, Word, State), !,
    State = newword,
    readWords(InStream, [Word | Acc], Ret).

    
readWord(InStream, W, State):-
        get_code(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream, State),
        convertWord(Chars, Clean),
        atom_codes(W,Clean).
        
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

checkCharAndReadRest(Char,[Char|Chars],InStream, State):-
        get_code(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream, State).