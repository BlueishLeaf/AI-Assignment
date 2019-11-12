% REPRESENTATION
% Source
connected_to(c0, [cs1, c133]).

% Right Branch
connected_to(cs1, [c101, lab1]). % TODO: Add lab1
connected_to(c101, [r101, c103]). % TODO: Add lab1
connected_to(r101, []).
connected_to(c103, [r103, c105, lab2]). % TODO: Add lab2
connected_to(r103, []).
connected_to(c105, [r105, c107]).
connected_to(r105, []).
connected_to(c107, [r107, c109]). % TODO: Add lab2
connected_to(r107, []).
connected_to(c109, [r109, c113, c111]).
connected_to(r109, []).
connected_to(c111, [r111]).
connected_to(r111, []).
connected_to(c113, [r113, c115]).
connected_to(r113, []).
connected_to(c115, [r115, c117]).
connected_to(r115, []).
connected_to(c117, [r117, c118]).
connected_to(r117, []).

% Left Branch
connected_to(c133, [c132]).
connected_to(c132, [c131]).
connected_to(c131, [r131, c129]).
connected_to(r131, []).
connected_to(c129, [r129, c127]). % TODO: Add lab4
connected_to(r129, []).
connected_to(c127, [r127, c125]).
connected_to(r127, []).
connected_to(c125, [r125, c123, lab4]). % TODO: Add lab4
connected_to(r125, []).
connected_to(c123, [r123, c121, lab3]). % TODO: Add lab3
connected_to(r123, []).
connected_to(c121, [r121, c119]).
connected_to(r121, []).
connected_to(c119, [r119, c118]).
connected_to(r119, []).

% Branches Converge on c118
connected_to(c118, [cc118, c119]).
connected_to(cc118, [canteen]).
connected_to(canteen, []).

% No clue what to do with labs lool
connected_to(lab1, [c101, lab2]).
connected_to(lab2, [c107, lab3]).
connected_to(lab3, [lab2]).
connected_to(lab4, [c129, lab1]).

% has_door(A, B, C) is true if a door exists between node A and B.
% C is the state of the door (open, closed, locked).
has_door(A, B, C) :-
    door(A, B, C);
    door(B, A, C).

%door(cs1, lab1, locked).
%door(c101, lab1, locked).
door(c101, r101, locked).
door(c131, r131, locked).

% cost is all sorts of fucked up
cost(A, B, C) :-
    has_door(A, B, open),
    cost(move, C).

cost(A, B, C) :-
    has_door(A, B, closed),
    cost(open, C).

cost(A, B, C) :-
    has_door(A, B, locked),
    cost(unlock_and_open, C).

cost(A, B, C) :-
    \+ has_door(A, B, _),
    cost(move, C).

cost(unlock, 2).
cost(open, 3).
cost(unlock_and_open, 5).
cost(move, 1).

goal(canteen).

% search(M,F,V) is true if there is a path from F to a goal node.
%  M is the search method. It is one of {depth,breadth}
%  V is the list of all nodes expanded (not just on the path found).

% This traces the frontier as the search proceeds.

% To seach from a node o103, issue the query:
% ? search(depth,[o103],S).
% ? search(breadth,[o103],S).
search(M,F,[N]) :-
   writeln(['Frontier: ',F]),
   select(M,N,F,_),
   goal(N).
search(M,F,[N|P]) :-
   select(M,N,F,F1),
   connected_to(N,NN),
   add_to_frontier(M,NN,F1,F2),
   search(M,F2,P).

% select(M,E,F,NF) is true if E is an element of frontier F and NF is
%   the remaining frontier after E is removed. M is the search method used.
% In each of these the frontier is the list of elements in order they
%   are to be chosen.
select(_,N,[N|F],F).

% add_to_frontier(M,N,F1,F2) is true if when using search method M, when
%   nodes N are added to frontier F1, the resulting frontier is list F2.
add_to_frontier(depth,N,F1,F2) :- !,
   append(N,F1,F2).

add_to_frontier(breadth,N,F1,F2) :- !,
   append(F1,N,F2).

% append(A,B,R) is true if R is the list containing the elements of A followed by the elements of B
append([],R,R).
append([H|T],L,[H|R]) :-
   append(T,L,R).

% writeln(L) is true if L is a list of items to be written on a line, followed by a newline.
writeln(L) :- writel(L),nl.

writel([]).
writel([H|T]) :- write(H), writel(T).
