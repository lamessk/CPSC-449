/*
** Starter code for Q6
*/
male(bob).
male(charles).
male(evan).
male(gordon).
female(alice).
female(diane).
female(fiona).
female(harriot).

likes(bob, pizza).
likes(evan, pizza).
likes(charles, alice).
likes(evan, fiona).
likes(gordon, fiona).
likes(fiona, charles).
likes(fiona, evan).
likes(charles, golf).
likes(evan, golf).
likes(gordon, golf).
likes(alice, shopping).
likes(diane, shopping).
likes(diane, bob).
likes(fiona, golf).
likes(harriot, golf).
likes(diane, steak).
likes(fiona, steak).

% Answer to question 6 

canBeFriends(F1, F2) :-
  female(F1), 
  female(F2), 
  F1 \= F2, 
  likes(F1, X), 
  likes(F2, X), 
  \+male(X). 
  
canBeFriends(F1, F2) :-
  male(F1), 
  male(F2), 
  F1 \= F2, 
  likes(F1, X), 
  likes(F2, X), 
  \+female(X). 
  
canBeFriends(F1, F2) :- 
  ((male(F1), female(F2)); (female(F1), male(F2))), 
  likes(F1, X), 
  likes(F2, X), 
  \+likes(F1, F2), 
  \+likes(F2, F1). 

% Positive - offset from beginning starting at 0 
% Negative - indexing from end of the list, with -1 as the last element 
elementAt([H | _], 0, H). 
elementAt(List, Num, Result) :- 
  List = [_ | T], 
  Num > 0, 
  N is Num - 1, 
  elementAt(T, N, Result).  
elementAt(List, Num, Result) :- 
  List = [_ | T], 
  Num < -1, 
  N is Num + 1,
  append(AllButLast, [_], List), 
  elementAt(AllButLast, N, Result). 
elementAt(List, -1, Result) :- 
  append(_, [X], Values), 
  Result = X.   
  





















