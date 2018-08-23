bigger(car, mouse). 
bigger(shoe, mouse). 
bigger(house, car). 
bigger(moon, house). 

% Bigger function, apply transitivity 
% Z is an undefined variable that prolog finds a solution for
% Take Z and do a recursive call 
% Prolog does brute force and fills in values for uninstantiated variables
biggerThan(X, Y) :- bigger(X, Y). 
biggerThan(X, Y) :-
  bigger(X, Z), 
  biggerThan(Z, Y). 
  
enemyl(tom, frank).
enemyl(frank, steve). 
enemyl(tom, lucy). 
friendl(tom, lucy). 
friendl(sarah, lucy). 

% determine reflexive relationship
% tries both directions
enemy(X, Y) :- enemyl(Y, X).
enemy(X, Y) :- enemyl(X, Y).  

friend(X, Y) :- friendl(Y, X). 
friend(X, Y) :- friendl(X, Y). 

% the enemy of my enemy is my friend
friend(X, Y) :- 
  enemy(X, Z), 
  enemy(Y, Z), 
  X \= Y, 
  X \= Z, 
  Y \= Z.

%Strings are lists of characters just like in Haskell 
% Simplest example of last element is a head with tail being empty 
% Equal (=) means matched 
lastElement([A | []], B) :- A = B. 
lastElement([_ | T], B) :- lastElement(T, B).

%remove last element 
removeLast([_ | []], []).
removeLast([A|B], Y) :- removeLast(B, Z), Y = [A|Z]. 

palindrome([]). 
palindrome([ _ | []]).
palindrome([A|B]) :- lastElement(B,A), removeLast(B,X), palindrome(X).  

%take length of a list 
% is means arithmetic equals 
% (=) means match 
lengthList([], 0). 
lengthList([_| T], Length) :- lengthList(T, Tail), Length is Tail + 1.
