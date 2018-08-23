fruit(grape).
fruit(plum). 
fruit(apple).
fruit(pear).
fruit(banana).
fruit(tomato).
fruit(bell_pepper). 
fruit(egg_plant).

vegtable(carrot).
vegtable(celery).
vegtable(tomato). 
vegtable(bell_pepper). 
vegtable(egg_plant).
% Chips are double classified as vegtables and junkfood. 
% In the rule, if checks if chips are matching junkfood and then it succeeds, so it won't go down to the _ rule in 
% okay for school and succeed. 
vegtable(fries). 
vegtable(chips). 

%healthy(grape) :- fruit(grape).
%healthy(plum) :- fruit(plum).
%healthy(apple) :- fruit(apple).
%healthy(pear) :- fruit(pear).
%healthy(banana) :- fruit(banana).
%healthy(carrot) :- vegtable(carrot).
%healthy(celery) :- vegtable(celery).
healthy(X) :- fruit(X).
healthy(X) :- vegtable(X).

causes_confusion(X) :- 
  fruit(X),
  vegtable(X).

okForFruitSalad(X) :- 
  fruit(X),
  \+vegtable(X).

has(alice, grape).
has(bob, apple).
has(bob, plum).
has(charles, apple).
has(charles, pear). 
has(diane, plum).
has(diane, celery).
has(diane, carrot). 

nut(peanut). 
nut(cashew). 
nut(almond). 

junkfood(chips). 
junkfood(candy). 
junkfood(fries). 

% If we get a nut, do not backtrack/ leave the rule. Fail. Guarantees that we do not process the rest of the rules. 
% Stop from backtracking, commit to choice. Don't go back and try to generate answers from other rules.
% Same for junkfood. 
% Fail is a built in prolog predicate 
% Example, if we have chips, it will go to rule and ask, is chips a nut? No, backtracks, goes to next rule, 
% and says is chips junkfood? Yes. It will succeed and get to !, and then it cannot backtrack, and will execute the fail.  

% if we have vegtable rule first, it will respond yes because in sequence it finds a success at vegtable
% But introduces issue of responding twice, because _ also evaluates. 

%okForSchool(X) :- vegtable(X). 
okForSchool(X) :- nut(X), !, fail.
okForSchool(X) :- junkfood(X), !, fail.
okForSchool(_).








 