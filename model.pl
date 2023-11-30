:- module(model, [create_model/0, create_model/1, create_model/2, query/2]).

:- use_module(attributes, [author/1, attribute/1, uses/2]).
:- use_module(utils, [map/3, make_symbol/2, lookup_symbol/4]).
:- use_module(classify, [extract_attributes/1]).


create_model :- create_model(Model), writeln(Model).

create_model(Model) :- extract_attributes(In_text), create_model(In_text, Model).
create_model(In_text, model(Symbols, Preds)) :-

    % create symbols
    findall(Author, author(Author), Authors),
    map(make_symbol, Authors, Author_symbols),
    findall(Attribute, attribute(Attribute), Attributes),
    map(make_symbol, Attributes, Attribute_symbols), !,
    append(Authors, Attributes, Arity_0),
    append(Author_symbols, Attribute_symbols, Symbols),

    % 0 place preds
    preds_0(Arity_0, Symbols, Preds_0),

    % 1 place preds
    Auhor_preds = f(1, author, Author_symbols),
    Attribute_preds = f(1, attribute, Attribute_symbols),
    lookup_symbols(In_text, Arity_0, Symbols, In_text_syms),
    In_text_preds = f(1, in_text, In_text_syms),


    % 2 place preds
    preds_2(Authors, Arity_0, Symbols, Pairs),
    Uses = f(2, uses, Pairs),
    make_self(Symbols, Selves),
    Self = f(2, self, Selves),

    % construction
    append(Preds_0, [Auhor_preds, Attribute_preds, In_text_preds], Preds_01),
    append(Preds_01, [Uses, Self], Preds).


preds_0([], [], []).
preds_0([A|Arity_0], [S|Symbols], Preds_0) :- 
    preds_0(Arity_0, Symbols, P), Preds_0 = [f(0, A, S)|P].

lookup_symbols([I|IS], Items, Symbols, [Sym|Rest]):- lookup_symbol(I, Items, Symbols, Sym), lookup_symbols(IS, Items, Symbols, Rest).
lookup_symbols([], _, _, []).

preds_2([], _, _, []).
preds_2([A|Authors], Arity_0, Symbols, Preds_2) :- 
    findall(Attribute, uses(A, Attribute), Attributes),
    make_symbol_pairs(A, Attributes, Arity_0, Symbols, Pairs),
    preds_2(Authors, Arity_0, Symbols, P2),
    append(Pairs, P2, Preds_2).

make_symbol_pairs(_, [], _, _, []).
make_symbol_pairs(Author, [U|Uses], Arity_0, Symbols, [Pair|Pairs]) :-
    make_symbol_pair(Author, U, Arity_0, Symbols, Pair),
    make_symbol_pairs(Author, Uses, Arity_0, Symbols, Pairs).

make_symbol_pair(Author, Use, Arity_0, Symbols, (A_Sym, U_Sym)) :-
    lookup_symbol(Author, Arity_0, Symbols, A_Sym),
    lookup_symbol(Use, Arity_0, Symbols, U_Sym).

make_self([], []).
make_self([S|Syms], [(S, S)|Pairs]) :- make_self(Syms, Pairs).


% predicate for constructing a query

query(Query) :- query(Query, 0).
query(Query, 0) :- 
    Query = and(
        author(X),
        all(Y,
            % for all Y, if X uses Y, then Y is in the text
            imp(
                and(attribute(Y), uses(X, Y)),
                in_text(Y)
            )
        )
    ).
query(Query, 1) :- 
    Query = and(
        author(X),
        some(L,
            and(
                % There exists some attribute L that X uses such that... 
                and(attribute(Y), uses(X, Y)),
                all(Y,
                    % For all Y, if X uses Y AND Y != L, then Y is in the text
                    imp(
                        and(
                            and(attribute(Y), uses(X, Y)),
                            not(self(Y, L))
                        ),
                        in_text(Y)
                    )
                )
            )
        )
    ).
