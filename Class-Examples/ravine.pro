time(a, 1). 
time(b, 2). 
time(c, 5). 
time(d, 10). 

%
% state: 
%     What's on the left  (flashlight + people) 
%     What's on the right (flashlight + people)
%
% State State: state([a,b,c,d, light], []).
% End state:   state([], [_, _, _, _, _].


%
%Transition that moves two people and the flashlight from left side of ravine 
% to the right side of the ravine. 
% Note that @< does a character by character comparison 
transition(OldState, Action, Time, NewState) :- 
  OldState = state(OldLeft, OldRight), 
  member(P1, OldLeft), 
  member(P2, OldLeft), 
  %P1 @< P2, 
  P1 \= light, 
  P2 \= light, 
  member(light, OldLeft), 
  
  Action = walkOver(P1, P2, light), 
  time(P1, Time1), 
  time(P2, Time2), 
  max_list([Time1, Time2], Time), 
  subtract(OldLeft, [P1, P2, light], NewLeft),
  NewRight = [P1, P2, light | OldRight],
  NewState = state(NewLeft, NewRight).

%
% Transition that moves one person and the flashlight from the right side of the ravine to 
% left side of the ravine 
%
transition(OldState, Action, Time, NewState) :- 
  OldState = state(OldLeft, OldRight), 
  member(P, OldRight), 
  P \= light, 
  member(light, OldRight),
  
  Action = walkBack(P, light),   
  time(P, Time), 
  NewLeft = [P, light | OldLeft], 
  subtract(OldRight, [P, light], NewRight),   
  NewState = state(NewLeft, NewRight).

solve(state([], [_, _, _, _, _]), [], 0). 
solve(CurrentState, Actions, Time) :- 
  transition(CurrentState, A, T, NewState), 
  solve(NewState, NewActions, NewTime), 
  Actions = [A | NewActions], 
  Time is T + NewTime. 
  
bestAnswer(State, ans(B, MinTime)) :- 
  findall(Time, solve(State, _, Time), TimeList), 
  min_list(TimeList, MinTime),
  findAll(A, solve(State, A, MinTime), Best), 
  member(B, Best). 
