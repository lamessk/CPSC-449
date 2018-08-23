%city(edmonton). 
%city(grand_prairie). 
% ... 
% Don't need to state what each of the cities are
% in the facts we kind of get these for free
% 

%Road between (point A, point B)
road(grand_prairie, edmonton). 
road(edmonton, lloydminister). 
road(edmonton, camrose). 
road(edmonton, red_deer). 
road(camrose, red_deer). 
road(red_deer, calgary). 
road(calgary, banff).
road(calgary, medicine_hat). 
road(calgary, lethbridge). 
road(medicine_hat, lethbridge). 

%Make roads bidirectional 
%roads(X,Y) :- roads(Y,X). 

% Base cases for the path rule
path(A, B, _, Route) :- road(A, B), Route = [A, B].
path(A, B, _, Route) :- road(B, A), Route = [B, A].

%recursive case for the path rule 
path(A, B, Prohibited, Route) :- 
  road(A, C), 
  \+member(C, Prohibited),
  path(C, B, [C | Prohibited], RouteCB ),
  Route = [A | RouteCB]. 
path(A, B, Prohibited, Route) :- 
  road(C, A),
  \+member(C, Prohibited),
  path(C, B, [C | Prohibited], RouteCB),
  Route = [A | RouteCB].