road(grand_prairie, edmonton). 
road(edmonton, lloydminster). 
road(edmonton, red_deer). 
road(edmonton, camrose). 
road(red_deer, camrose). 
road(red_deer, calgary). 
road(calgary, banff). 
road(calgary, medicine_hat). 
road(calgary, lethbridge). 
road(medicine_hat, lethbridge).

path(A, B, _, Route) :- road(A, B), Route = [A, B]. 
path(A, B, _, Route) :- road(B, A), Route = [B, A].

path(A, B, Prohobited, Route) :- 
  road(A, C),
  \+member(C, Prohobited), 
  path(C, B, [C | Prohobited], RouteCB), 
  Route = [A | RouteCB].
  
path(A, B, Prohobited, Route) :- 
  road(C, A),
  \+member(C, Prohibited), 
  path(C, B, [C | Prohibited], RouteCB),
  Route = [A | RouteCB]. 