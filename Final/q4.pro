intList(_, _, 0, X) :- X = [], !. 
intList(Lower, Upper, Skipped, Result) :- 
  Upper =< Lower, 
  Result = [].   
intList(Lower, Upper, Skipped, Result) :- 
  Lower < Upper,
  Skip is Lower + Skipped, 
  intList(Skip, Upper, Skipped, X),
  Result = [Lower | X].
