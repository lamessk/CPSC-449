%
% start state: state(atDoor, atWindow, onFloor, hasNot)
% End State: state(_, _, _, has)
%
transition(state(atMiddle, atMiddle, onBox, hasNot), 
           grasp,
		   state(atMiddle, atMiddle, onBox, has)).

transition(state(X, X, onFloor, B), 
          climb, 
          state(X, X, onBox, B)).

transition(state(X, X, onFloor, B), 
           carry, 
           state(Y, Y, onFloor, B)). 

transition(state(_, BoxLoc, onFloor, B), 
           walk, 
           state(_, BoxLoc, onFloor, B)).

% Must be recursive because we need to move through states
% Base case is the simplest one, where we have the banana. 
canGet(state(_, _, _, has), ActionList) :- ActionList = []. 
% Recursive case, can we get from current state to a state where we have the banana
% Can we move from current state to a new state in one action, and can we get banana from the new state 
%Cut off the option to backtrack. First solution is good and stops silly answers from occuring (like walking around a lot)
canGet(CurrentState, ActionList) :- 
  transition(CurrentState, A, NewState), 
  canGet(NewState, Actions), 
  ActionList = [A | Actions], !. 