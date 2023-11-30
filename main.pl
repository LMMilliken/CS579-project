:- use_module(classify, [classify/1, extract_attributes/1]).
:- use_module(model, [create_model/0, create_model/1, create_model/2, query/2]).
:- use_module(cnt, [load_syllables/0]).
:- use_module(modelChecker2, [satisfy/4]).

:- initialization(load_syllables).