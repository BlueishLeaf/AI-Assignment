% DYNAMIC PREDICATES
:- dynamic item/2.
:- dynamic robot/3.
:- dynamic door/3.

% ROBOT
% robot(L, P, [I]).
% L is the robot's location
% P is the robot's remaining power
% I is the robot's inventory
robot(c0, 150, []).

% ROBOT ACTIONS
pick_up(Object) :-
  robot(Location, _, _),
  item(Object, Location),
  retract(item(Object, Location)),
  retract(robot(L,P,I)),
  assert(robot(L,P,[Object|I])).

unlock_door(A, B) :-
  robot(A, _, _),
  connected(A, B),
  connection(A, B, locked),
  retract(connection(A, B, locked)),
  retract(robot(L, P, I)),
  assert(robot(L, P - 2, I)),
  assert(connection(A, B, closed)).

open_door(A, B) :-
  robot(A, _, _),
  connected(A, B),
  connection(A, B, closed),
  retract(connection(A, B, closed)),
  assert(connection(A, B, open)).

move(A, B) :-
  can_move(A, B),
  retract(robot(_, P, H)),
  assert(robot(B, P, H)).

can_move(A, B) :-
  robot(A, _, _),
  connected(A, B),
  connection(A, B, open),

item(key, r117).
item(coffee, canteen).

connection(c0, cs1, open).
connection(cs1, c101, open).
connection(c101, c103, open).
connection(c103, c105, open).
connection(c105, c107, open).
connection(c107, c109, open).
connection(c109, c111, open).
connection(c109, c113, open).
connection(c113, c115, open).
connection(c115, c117, open).
connection(c117, c118, open).
connection(c118, c119, open).
connection(c119, c121, open).
connection(c121, c123, open).
connection(c123, c125, open).
connection(c125, c127, open).
connection(c127, c129, open).
connection(c129, c131, open).
connection(c131, c132, open).
connection(c132, c133, open).
connection(c133, c0, open).
connection(c118, cc118, open).
connection(cs1, lab1, locked).
connection(c101, lab1, locked).
connection(c103, lab2, locked).
connection(c107, lab2, locked).
connection(c123, lab3, locked).
connection(c125, lab4, locked).
connection(c129, lab4, locked).
connection(lab1, lab2, open).
connection(lab1, lab4, locked).
connection(lab2, lab3, locked).
connection(c101, r101, locked).
connection(c103, r103, locked).
connection(c105, r105, locked).
connection(c107, r107, locked).
connection(c109, r109, locked).
connection(c111, r111, locked).
connection(c113, r113, locked).
connection(c115, r115, locked).
connection(c117, r117, open).
connection(c119, r119, locked).
connection(c121, r121, locked).
connection(c123, r123, locked).
connection(c125, r125, locked).
connection(c127, r127, locked).
connection(c129, r129, locked).
connection(c131, r131, locked).
connection(cc118, canteen, locked).

connected(A, B) :- connection(A, B, _).
connected(A, B) :- connection(B, A, _).

cost(open, 1).
cost(closed, 3).
cost(locked, 5). % 2 to unlock plus 3 to open
cost(A, B, C) :-
  connection(A, B, DS), % Get door state to determine cost of node
  cost(DS, C).

%--------------------------------------------------------------%
%   Uniform Cost Search                                        %
%   call: uni_cost(+[[Start]],+Goal,-Path,-ExploredNodes).     %
%--------------------------------------------------------------%
uni_cost([[Goal|Path]|_],Goal,[Goal|Path],0).
uni_cost([Path|Queue],Goal,FinalPath,N) :- 
  extend(Path,NewPaths),
  append(Queue,NewPaths,Queue1), 
  sort_queue(Queue1,NewQueue),
  uni_cost(NewQueue,Goal,FinalPath,M),
  N is M+1.

sort_queue(L,L2) :-
  swap(L,L1), !,
  sort_queue(L1,L2).
sort_queue(L,L).

swap([X,Y|T],[Y,X|T]) :-
  reverse_path_cost(X,CX),
  reverse_path_cost(Y,CY),
  CX>CY.
swap([X|T],[X|V]) :-
  swap(T,V).

path_cost([A,B],Cost) :-
  cost(A,B,Cost).
path_cost([A,B|T],Cost) :-
  cost(A,B,Cost1),
  path_cost([B|T],Cost2),
  Cost is Cost1+Cost2.

reverse_path_cost([A,B],Cost) :-
  cost(B,A,Cost).
reverse_path_cost([A,B|T],Cost) :-
  cost(B,A,Cost1),
  reverse_path_cost([B|T],Cost2),
  Cost is Cost1+Cost2.

extend([Node|Path],NewPaths) :-
  findall([NewNode,Node|Path],
    (connection(Node,NewNode,_), 
    \+ member(NewNode,Path)),
    NewPaths).

% writeln(L) is true if L is a list of items to be written on a line, followed by a newline.
writeln(L) :- writel(L),nl.

writel([]).
writel([H|T]) :- write(H), writel(T).
