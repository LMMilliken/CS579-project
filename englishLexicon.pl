/*************************************************************************

    File: englishLexicon.pl
    Copyright (C) 2004,2005,2006 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.3 (November 2006).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

/*========================================================================
   Determiners
========================================================================*/

lexEntry(det,[syntax:[every],mood:decl,type:uni]).
lexEntry(det,[syntax:[a],mood:decl,type:indef]).
/*
lexEntry(det,[syntax:[the],mood:decl,type:def]).
Maybe here is where we should put the link to the FOL formula calcualted
lexEntry(det,[syntax:[this],mood:decl,type:def]).*/
lexEntry(det,[syntax:[which],mood:int,type:wh]).


/*========================================================================
   Nouns
========================================================================*/

/*Added for the project*/
lexEntry(noun,[symbol:text,syntax:[text]]).
lexEntry(noun,[symbol:opera,syntax:[opera]]).
lexEntry(noun,[symbol:fragment,syntax:[fragment]]).

lexEntry(noun,[symbol:author,syntax:[author]]).
lexEntry(noun,[symbol:writer,syntax:[writer]]).


/*========================================================================
   Proper Names
========================================================================*/

lexEntry(pn,[symbol:mia,syntax:[mia]]).
lexEntry(pn,[symbol:honey_bunny,syntax:[honey,bunny]]).


/*========================================================================
   Quantified Noun Phrases
========================================================================*/

lexEntry(qnp,[symbol:author,syntax:[who],mood:int,type:wh]).
lexEntry(qnp,[symbol:thing,syntax:[what],mood:int,type:wh]).


/*========================================================================
   Wtt
========================================================================*/

lexEntry(wtt, [symbol: wtt, syntax:[wrote, the, text]]).

/*========================================================================
   Transitive Verbs
========================================================================*/

lexEntry(tv,[symbol:have,syntax:[have],inf:inf,num:sg]).
lexEntry(tv,[symbol:have,syntax:[has],inf:fin,num:sg]).
lexEntry(tv,[symbol:have,syntax:[have],inf:fin,num:pl]).

/*Added for the project  */
lexEntry(tv,[symbol:write,syntax:[write],inf:inf,num:sg]).
lexEntry(tv,[symbol:write,syntax:[writes],inf:fin,num:sg]).
lexEntry(tv,[symbol:write,syntax:[write],inf:fin,num:pl]).

lexEntry(tv,[symbol:wrote,syntax:[wrote],inf:inf,num:sg]).
lexEntry(tv,[symbol:wrote,syntax:[wrote],inf:fin,num:sg]).
lexEntry(tv,[symbol:wrote,syntax:[wrote],inf:inf,num:pl]).
lexEntry(tv,[symbol:wrote,syntax:[wrote],inf:fin,num:pl]).

/*========================================================================
   Copula
========================================================================*/

lexEntry(cop,[pol:pos,syntax:[is],inf:fin,num:sg]).
lexEntry(cop,[pol:neg,syntax:[is,not],inf:fin,num:sg]).
lexEntry(cop,[pol:pos,syntax:[are],inf:fin,num:pl]).
lexEntry(cop,[pol:neg,syntax:[are,not],inf:fin,num:pl]).


/*========================================================================
   Prepositions
========================================================================*/

lexEntry(prep,[symbol:about,syntax:[about]]).
lexEntry(prep,[symbol:in,syntax:[in]]).
lexEntry(prep,[symbol:of,syntax:[of]]).
lexEntry(prep,[symbol:with,syntax:[with]]).
lexEntry(prep,[symbol:without,syntax:[without]]).

/*========================================================================
   Adjectives
========================================================================*/

/*Added for the project */
lexEntry(adj,[symbol:copied,syntax:[copied]]).
lexEntry(adj,[symbol:pasted,syntax:[pasted]]).


/*========================================================================
   Relative Pronouns
========================================================================*/

lexEntry(relpro,[syntax:[who]]).
lexEntry(relpro,[syntax:[that]]).


/*========================================================================
   Coordinations
========================================================================*/
/*
lexEntry(coord,[syntax:[and],type:conj]).
lexEntry(coord,[syntax:[or],type:disj]).
*/


/*========================================================================
   Auxiliary Verbs
========================================================================*/

lexEntry(av,[syntax:[does],inf:fin,num:sg,pol:pos]).
lexEntry(av,[syntax:[does,not],inf:fin,num:sg,pol:neg]).
lexEntry(av,[syntax:[did],inf:fin,num:sg,pol:pos]).
lexEntry(av,[syntax:[did,not],inf:fin,num:sg,pol:neg]).


