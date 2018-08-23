%adapter(startval, endval). 

connect(End, Start, _, ListAdapt) :- 
  Start = End, 
  ListAdapt = []. 
  
connect(End, Start, Adapters, ListAdapt) :- 
  Start \= End, 
  Adapters = [adapter(Start1, End1) | T],
  Start1 = End, 
  Start = End1,
  connect(End1, Start, T, Inter),   
  ListAdapt = [adapter(Start1, End1) | Inter]. 
  
connect(End, Start, Adapters, ListAdapt) :- 
  Start \= End, 
  Adapters = [adapter(Start1, End1) | T],
  connect(End, Start, T, ListAdapt). 
  
bestConnection(End, Start, Adapters, Best) :- 
  findall(AList, connect(End, Start, Adapters, AList). 