:- module(attributes, [author/1, attribute/1, uses/2]).
author(shakespeare).
author(elizabeth_bishop).
% author(auth2).

attribute(iambic_pentameter).

attribute(lines_14).
attribute(lines_19).


attribute(english_sonnet).
% attribute(quartine).
% attribute(mono).
% attribute(kissed).
% attribute(concatenated).
% attribute(incatenated).
attribute(villenelle).

uses(shakespeare, iambic_pentameter).
uses(shakespeare, lines_14).
uses(shakespeare, english_sonnet).

uses(elizabeth_bishop, iambic_pentameter).
uses(elizabeth_bishop, lines_19).
uses(elizabeth_bishop, villenelle).

% uses(auth2, iambic_pentameter).
% uses(auth2, mono).