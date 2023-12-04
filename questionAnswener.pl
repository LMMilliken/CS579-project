/*========================================================================
    Answer Questions
========================================================================*/

answerQuestion(que(X,R,S),Model):-
    satisfy(some(X,and(R,S)),Model,[],Result),
    \+ Result=undef.
