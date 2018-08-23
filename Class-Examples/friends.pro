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

canBeFriends(F1, F2) :- 
  female(F1), 
  female(F2), 
  likes(F1, X), 
  likes(F2, X),
  F1 \= F2, 
  \+male(X).
  
canBeFriends(F1, F2) :- 
  male(F1), 
  male(F2),
  likes(F1, X), 
  likes(F2, X),
  F1 \= F2,   
  \+female(X). 

canBeFriends(F1, F2) :- 
  male(F1), 
  female(F2), 
  likes(F1, X), 
  likes(F2, X),
  \+likes(F1, F2),
  \+likes(F2, F1),    
  F2 \= X,
  F1 \= X,
  F1 \= F2. 

elementAt([H | T], 0, X) :- X = H. 
elementAt(List, Num, X) :- 
  Num > 0, 
  List = [ H | T],
  N is Num - 1,   
  elementAt(T, N, X). 
elementAt(List, -1, X) :- last(List, X). 
elementAt(List, Num, X) :- 
  Num < 0, 
  reverse(List, Y), 
  Y = [ H | T],
  N is Num + 1,   
  elementAt(T, N, X). 





