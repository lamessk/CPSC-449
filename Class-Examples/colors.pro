red(apples).
red(rubys).

orange(carrot). 
orange(pumpkin). 
orange(orange). 

green(celery). 
green(emeralds). 
green(leaf).
green(green_bean). 


%without cuts, color(apples,X). says yes, then tries to continue looking, and returns unknown. 
%If we stick cuts inside the rule, we tell it to find exactly one solution and not look further. 
color(X, red) :- red(X), !. 
color(X, orange) :- orange(X), !. 
color(X, green) :- green(X), !. 
color(X, unknown). 