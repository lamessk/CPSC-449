male(abraham). 
male(clancy). 
male(herb). 
male(homer). 
male(bart). 

female(mona). 
female(jackeline). 
female(marge). 
female(patty). 
female(selma). 
female(lisa). 
female(maggie). 
female(ling). 
female(gaby). 
female(edwina).
female(abbie).

%
% hasParent(child, parent). 
%
hasParent(bart, homer). 
hasParent(bart, marge). 
hasParent(lisa, homer). 
hasParent(lisa, marge). 
hasParent(maggie, homer). 
hasParent(maggie, marge). 
hasParent(ling, selma).
hasParent(herb, gabby). 
hasParent(herb, abraham). 
hasParent(abbie, edwina). 
hasParent(abbie, abraham).
hasParent(homer, mona). 
hasParent(homer, abraham). 
hasParent(marge, clancy). 
hasParent(marge, jackeline).
hasParent(patty, clancy). 
hasParent(patty, jackeline).
hasParent(selma, clancy). 
hasParent(selma, jackeline).

hasFather(Child, Father) :-
  hasParent(Child, Father), 
  male(Father).

hasMother(Child, Mother) :-
  hasParent(Child, Mother), 
  female(Mother).
  
hasFullSibling(Child, Sibling) :- 
  hasFather(Child, X), 
  hasFather(Sibling, X), 
  hasMother(Child, Y),
  hasMother(Sibling, Y), 
  X \= Y.   

hasGrandMother(Child, Grandma) :- 
  hasParent(Child, X), 
  hasParent(X, Grandma), 
  female(Grandma). 

hasBrother(S1, S2) :- 
  hasFullSibling(S1, S2), 
  male(S2). 
  
hasAunt(Child, Aunt) :- 
  hasParent(Child, X), 
  hasFullSibling(X, Aunt), 
  female(Aunt). 

hasCousin(C1, C2) :- 
  hasGrandMother(C1, X), 
  hasGrandMother(C2, X), 
  C1 \= C2. 

hasAncestor(Child, Ancestor) :- 
  hasParent(Child, Ancestor). 
hasAncestor(Child, Ancestor) :- 
  hasParent(Child, Parent), 
  hasAncestor(Parent, Ancestor). 


