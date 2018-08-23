countOccurance(_, [], 0). 
countOccurance(X, List, Count) :- 
  List = [Head | Tail], 
  X = Head, 
  countOccurance(X, Tail, TailCount), 
  Count is TailCount + 1.
countOccurance(X, List, Count) :- 
  List = [Head | Tail], 
  X \= Head, 
  countOccurance(X, Tail, Count).

% Two ways t stop infinite recurcusion: Cuts in the base cases
% Having N > 1 in the recursive case to make sure it doesnt go negative. 
% Caution, don't always use cuts, there are sometimes ways to go without it. 
nFib(0, 0) :- !. 
nFib(1, 1) :- !. 
nFib(N, Result) :- 
  %N > 1,
  Y is N - 1, 
  X is N - 2, 
  nFib(Y, ResultY), 
  nFib(X, ResultX), 
  Result is ResultY + ResultX. 