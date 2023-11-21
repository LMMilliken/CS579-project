:- module(profile, [haiku/2]).
:- use_module(cnt, [ns/2, load_syllables/1]).

syl(N) --> [X], {ns(X, N)}.
syl(N) --> [(N, _)].

haiku --> syl(5), syl(7), syl(5).

aN(0) --> [].
aN(N) --> [a], aN(X), {N is X + 1}.