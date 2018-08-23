% Without cuts
% Ineffieicent, we have to search the list twice.

isFavorite(X, Message) :- 
  member(X, [2, 4, 8, 16, 32, 64, 128, 256]), 
  Message = 'It is one of my favorite numbers!'.
isFavorite(X, Message) :- 
  \+member(X, [2, 4, 8, 16, 32, 64, 128, 256]), 
  Message = 'It is *not* one of my favorite numbers.'.

% With cuts. 
% If we are a member, we are in the right rule,it will get to the cut and won't backtrack past ! anymore. 
% if not a member, wrong rule, prolog will reach member line, realize wrong rule and backtrack and try the second rule.  
% It CAN backtrack up to !, so if we had two messages, it would print one message, the backtrack and print the other. 
isFavorite(X, Message) :- 
  member(X, [2, 4, 8, 16, 32, 64, 128, 256]),
  !,   
  Message = 'It is one of my favorite numbers!'.
isFavorite(X, Message) :- 
  Message = 'It is *not* one of my favorite numbers.'.