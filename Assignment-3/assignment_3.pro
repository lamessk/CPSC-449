/*
** CPSC 449 - Assignment #3
** Spring 2018 Semester 
** 
** Author: Lamess Kharfan 
** Starter Code provided by Ben Stephenson
**
*/

/*
** Prerequisite information for non-computer science courses.
*/
prereqFor(data201, []).
prereqFor(data211, []).
prereqFor(data311, [data201 | [X]]) :-
  member(X, [data211, cpsc217, cpsc231, cpsc235, engg233]).
prereqFor(math211, []).
prereqFor(math213, []).
prereqFor(math249, []).
prereqFor(math265, []).
prereqFor(math267, [X]) :- member(X, [math249, math265, math275]).
prereqFor(math271, [X]) :- member(X, [math211, math213]).
prereqFor(math273, []).
prereqFor(math275, []).
prereqFor(math277, [math211, math275]).
prereqFor(math311, [X]) :- member(X, [math211, math213]).
prereqFor(math313, [math213]).
prereqFor(math315, [X]) :- member(X, [math271, math273]).
prereqFor(math331, [X, Y]) :- member(X, [math211, math213]), member(Y, [math267, math277]).
prereqFor(math376, [X]) :- member(X, [math267, math277]).
prereqFor(stat205, []).
prereqFor(stat213, []).
prereqFor(stat321, [X]) :- member(X, [math267, math277]).
prereqFor(phil279, []).
prereqFor(phil377, []).
prereqFor(encm335, [engg233]).
prereqFor(encm369, [enel353 | [X]]) :- member(X, [encm335, ensf337]).
prereqFor(ensf337, [engg233]).
prereqFor(engg233, []).
prereqFor(seng300, [X]) :- member(X, [cpsc319, cpsc331]).
prereqFor(enel353, X) :- member(X, [[admit_ENEL_or_ENSF], [cpsc233, math271]]).
prereqFor(admit_ENEL_or_ENSF, []).

/*
** Courses that require consent of the department should include consent
** followed by the course number in their prerequisite lists.
*/
prereqFor(consent235, []).
prereqFor(consent399, []).
prereqFor(consent499, []).
prereqFor(consent502, []).
prereqFor(consent503, []).
prereqFor(consent527, []).
prereqFor(consent528, []).
prereqFor(consent550, []).
prereqFor(consent568, []).
prereqFor(consent581, []).
prereqFor(consent585, []).
prereqFor(consent594, []).
prereqFor(consent598, []).
prereqFor(consent599, []).

/*
** Prerequisites for computer science courses that can only have their
** prerequisites satisfied in one way.
*/
prereqFor(cpsc203, []).
prereqFor(cpsc217, []).
prereqFor(cpsc231, []).
prereqFor(cpsc233, [cpsc231]).
prereqFor(cpsc235, [consent235]).
prereqFor(cpsc399, [consent399]).
prereqFor(cpsc405, [seng300]).
prereqFor(cpsc409, [cpsc355]).
prereqFor(cpsc499, [consent499]).
prereqFor(cpsc501, [cpsc449]).
prereqFor(cpsc502, [consent502]).
prereqFor(cpsc503, [consent503]).
prereqFor(cpsc511, [cpsc413]).
prereqFor(cpsc513, [cpsc313]).
prereqFor(cpsc517, [cpsc413]).
prereqFor(cpsc521, [cpsc313, cpsc449]).
prereqFor(cpsc522, [cpsc413]).
prereqFor(cpsc526, [cpsc441]).
prereqFor(cpsc527, [cpsc313, cpsc457, consent527]).
prereqFor(cpsc528, [cpsc313, cpsc457, consent528]).
prereqFor(cpsc550, [cpsc457, consent550]).
prereqFor(cpsc559, [cpsc441, cpsc457]).
prereqFor(cpsc561, [cpsc413]).
prereqFor(cpsc565, [cpsc433]).
prereqFor(cpsc567, [cpsc457, cpsc433]).
prereqFor(cpsc568, [cpsc433, consent568]).
prereqFor(cpsc571, [cpsc471]).
prereqFor(cpsc572, [cpsc571]).
prereqFor(cpsc575, [seng300]).
prereqFor(cpsc577, [cpsc453]).
prereqFor(cpsc581, [cpsc481, consent581]).
prereqFor(cpsc584, [cpsc481]).
prereqFor(cpsc585, [cpsc453, consent585]).
prereqFor(cpsc587, [cpsc453]).
prereqFor(cpsc589, [cpsc453]).
prereqFor(cpsc591, [cpsc453]).
prereqFor(cpsc594, [consent594]).
prereqFor(cpsc598, [consent598]).
prereqFor(cpsc599, [consent599]).

/*
** Part 1: Rules for all other computer science courses.
*/
prereqFor(cpsc219, [X]) :- 
  member(X, [cpsc217, data211]).
prereqFor(cpsc331, [M, C]) :- 
  member(M, [math271, math273]), 
  member(C, [cpsc219, cpsc233, cpsc235]).
prereqFor(cpsc313, [M, P, C]) :-
  member(M, [math271, math273]),
  member(P, [phil279, phil377]), 
  member(C, [cpsc219, cpsc233, cpsc235]).
prereqFor(cpsc319, [X]) :- 
  member(X, [cpsc219, cpsc233, cpsc235, encm335, ensf337]).
prereqFor(cpsc329, [X]) :- 
  member(X, [cpsc217, cpsc231, cpsc235, data211, engg233]).
prereqFor(cpsc335, [X]) :- 
  member(X, [cpsc319, cpsc331]).
prereqFor(cpsc355, [X]) :- 
  member(X, [cpsc219, cpsc233, cpsc235]).
prereqFor(cpsc359, [cpsc355 | [P]]) :- 
  member(P, [phil279, phil377]).  
prereqFor(cpsc411, [X]) :- 
  member(X, [cpsc319, cpsc331]).
prereqFor(cpsc413, [cpsc313, cpsc331 | [M, C]]) :- 
  member(M, [math211, math213]),
  member(C, [math249, math265, math275]). 
prereqFor(cpsc418, [cpsc331 | [X]]) :- 
  member(X, [math271, math273, math315]).
prereqFor(cpsc433, [cpsc313 | [X]]) :- 
  member(X, [phil279, phil377]).
prereqFor(cpsc441, [X, Y]) :- 
  member(X, [cpsc319, cpsc331]),
  member(Y, [cpsc359, encm369]).
prereqFor(cpsc449, [C, P]) :- 
  member(C, [cpsc319, cpsc331]), 
  member(P, [phil279, phil377]).
prereqFor(cpsc453, [C, X, Y]) :- 
  member(C, [cpsc319, cpsc331]),
  member(X, [math211, math213]), 
  member(Y, [math267, math277]).
prereqFor(cpsc457, [X, Y]) :-
  member(X, [cpsc319, cpsc331]), 
  member(Y, [cpsc359, encm369]).
prereqFor(cpsc461, [cpsc355 | [X]]) :-
  member(X, [cpsc319, cpsc331]).
prereqFor(cpsc471, [X]) :- 
  member(X, [cpsc319, cpsc331]). 
prereqFor(cpsc481, [X]) :- 
  member(X, [seng300, data311]).
prereqFor(cpsc491, [X, Y, Z]) :- 
  member(X, [cpsc319, cpsc331]), 
  member(Y, [math211, math213]),
  member(Z, [math249, math265, math275]). 
prereqFor(cpsc518, [cpsc413 | [X]]) :- 
  member(X, [math211, math213]).
prereqFor(cpsc519, [cpsc413 | [X]]) :- 
  member(X, [math311, math313]). 
prereqFor(cpsc525, [cpsc457 | [X]]) :- 
  member(X, [math271, math273]).
prereqFor(cpsc530, [X, Y, Z]) :-
  member(X, [cpsc219, cpsc233, cpsc235]),
  member(Y, [math271, math273, math315]),
  member(Z, [stat205, stat213, stat321]).
prereqFor(cpsc531, [cpsc457 | [X]]) :- 
  member(X, [stat205, stat213, stat321]). 
prereqFor(cpsc535, [X]) :- 
  member(X, [math311, math313, math331, math376]).
prereqFor(cpsc583, [X]) :- 
  member(X, [cpsc319, cpsc331, data311]). 
prereqFor(graphics, [ cpsc453 | [M, C1, C2, C3, C4, A]]) :- 
  member(M, [math267, math277]), 
  member(C1, [cpsc587, cpsc589, cpsc591]),
  member(C2, [cpsc587, cpsc589, cpsc591]), 
  C1 \= C2, sort([C1, C2], [C1,C2]),   
  member(C3, [cpsc481, cpsc535, cpsc585, cpsc587, cpsc589, cpsc591, cpsc599]),
  member(C4, [cpsc481, cpsc535, cpsc585, cpsc587, cpsc589, cpsc591, cpsc599]),
  C3 \= C4, sort([C3, C4], [C3, C4]),   
  member(A, [art231, art233, art235, art241, art349, art379, phys211, phys221, phys227]).

/*
** Courses that are antirequisite to each other.
*/
antireqs([cpsc217, cpsc231, cpsc235, data211, engg233]).
antireqs([cpsc219, cpsc233, cpsc235]).
antireqs([cpsc319, cpsc331]).
antireqs([cpsc355, encm369]).
antireqs([cpsc441, enel573]).
antireqs([cpsc471, btma331]).
antireqs([cpsc491, engg407]).
antireqs([cpsc502, cpsc503]).
antireqs([math211, math213]).
antireqs([math249, math265, math275]).
antireqs([math267, math277]).
antireqs([math311, math313]).
antireqs([math318, cpsc418]).
antireqs([math331, math367, math377]).
antireqs([math335, math355]).
antireqs([math361, math313]).
antireqs([math375, math376]).
antireqs([math391, cpsc491]).
antireqs([stat205, stat213, stat217, stat327]).
antireqs([phil279, phil377]).
antireqs([ensf480, seng300]).
antireqs([math271, math273]).


/*
** Part 2: allPrereqFor gets all combinations of courses that can  
** be used to satisfy the prerequistites for a course. Includes direct
** prerequisites and recurively identifies prerequisites of prerequistites.
*/
allPrereqFor(Course, AllSorted) :- 
  prereqFor(Course, X),
  prereqList(X, All),
  sort(All, AllSorted).  

% Helper function that recursively finds allPrereqFor head of the list 
% Recurses on tail of the list and 
prereqList([], []).
prereqList([H | T], Reqs) :-
  allPrereqFor(H, X),
  append([H], X, S),   
  prereqList(T, Y),
  append(S, Y, Reqs).

/*
** Part 3: allPrereqFor_NoAnti generates a list of all prerequisites for 
** a course (direct and indirect) such that generated lists never
** contain a pair of courses that are anti-requisites to one another.  
*/
allPrereqFor_NoAnti(Course, Y) :- 
  allPrereqFor(Course, X), 
  \+getAntiList(X), 
  sort(X,Y).
  
/* 
**getAntiList: Get two elements of the list, send to checkIfAnti. 
** make sure two elements we are grabbing from the list are not the same.
** Rule is true for two courses that are anti-requistites in the same list.  
*/
getAntiList(AllReqs) :- 
 member(X, AllReqs), 
 member(Y, AllReqs),
 X \= Y,  
 checkIfAnti(X, Y).

/* 
** Check if the two elements passed in are both in a list of antireqs. 
** If both are, rule is true.
*/
checkIfAnti(X, Y) :- 
  antireqs(Antis), 
  member(X, Antis), 
  member(Y, Antis). 

/*
** Part 4: neededCourses Identifies sets of courses that can be taken in
** order to allow enrollment in some desired course. Lists do not contain
** any anti-requistites. 
*/
neededCourses(Taken, Desired, Needed) :- 
  allPrereqFor_NoAnti(Desired, AllRequired),
  subtract(AllRequired, Taken, X),
  append(Taken, X, Y), 
  \+getAntiList(Y), 
  sort(X, Needed).  

/*
** Additional Challenge Attempt. 
** List all courses that are needed in addition to those taken 
** to satisfy the graphics concentration requirements.
*/
concentration(Taken, X) :- 
  prereqFor(graphics, AllGraphics), 
  subtract(AllGraphics, Taken, Y), 
  rec(Y, Taken, Z), 
  sort(Z, X).   

rec([], _, []). 
rec([H | T], Took, Ret) :- 
  neededCourses(Took, H, X), 
  rec(T, Took, Y), 
  append(X, Y, Ret). 
  
