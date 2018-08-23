elementAt([H|_], 0, H). 
elementAt([_|T], K, X) :- 
  K > 0,  
  NewK is K - 1, 
  elementAt(T, NewK, X).
  
countOccurances([], _, 0). 
countOccurances([H | T], N, Result) :- 
  N = H, 
  countOccurances(T, N, R), 
  Result is 1 + R.  
countOccurances([H | T], N, Result) :- 
  N \= H, 
  countOccurances(T, N, Result).   
  
