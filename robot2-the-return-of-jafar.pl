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
  is_connected(A, B),
  connected_to(A, B, locked),
  retract(connected_to(A, B, locked)),
  retract(robot(L, P, I)),
  assert(robot(L, P - 2, I)),
  assert(connected_to(A, B, closed)).

open_door(A, B) :-
  robot(A, _, _),
  is_connected(A, B),
  connected_to(A, B, closed),
  retract(connected_to(A, B, closed)),
  assert(connected_to(A, B, open)).

move(A, B) :-
  can_move(A, B),
  retract(robot(_, P, H)),
  assert(robot(B, P, H)).

can_move(A, B) :-
  robot(A, _, _),
  is_connected(A, B),
  connected_to(A, B, open).

item(key, r117).
item(coffee, canteen).

connected_to(c0, cs1, open).
connected_to(cs1, c101, open).
connected_to(c101, c103, open).
connected_to(c103, c105, open).
connected_to(c105, c107, open).
connected_to(c107, c109, open).
connected_to(c109, c111, open).
connected_to(c109, c113, open).
connected_to(c113, c115, open).
connected_to(c115, c117, open).
connected_to(c117, c118, open).
connected_to(c118, c119, open).
connected_to(c119, c121, open).
connected_to(c121, c123, open).
connected_to(c123, c125, open).
connected_to(c125, c127, open).
connected_to(c127, c129, open).
connected_to(c129, c131, open).
connected_to(c131, c132, open).
connected_to(c132, c133, open).
connected_to(c133, c0, open).
connected_to(c118, cc118, open).
connected_to(cs1, lab1, locked).
connected_to(c101, lab1, locked).
connected_to(c103, lab2, locked).
connected_to(c107, lab2, locked).
connected_to(c123, lab3, locked).
connected_to(c125, lab4, locked).
connected_to(c129, lab4, locked).
connected_to(lab1, lab2, open).
connected_to(lab1, lab4, locked).
connected_to(lab2, lab3, locked).
connected_to(c101, r101, locked).
connected_to(c103, r103, locked).
connected_to(c105, r105, locked).
connected_to(c107, r107, locked).
connected_to(c109, r109, locked).
connected_to(c111, r111, locked).
connected_to(c113, r113, locked).
connected_to(c115, r115, locked).
connected_to(c117, r117, open).
connected_to(c119, r119, locked).
connected_to(c121, r121, locked).
connected_to(c123, r123, locked).
connected_to(c125, r125, locked).
connected_to(c127, r127, locked).
connected_to(c129, r129, locked).
connected_to(c131, r131, locked).
connected_to(cc118, canteen, locked).
is_connected(A, B) :- connected_to(A, B, _).
is_connected(A, B) :- connected_to(B, A, _).

cost(open, 1).
cost(closed, 3).
cost(locked, 6). % 2 to unlock plus 3 to open plus 1 to move
cost_of(A, B, Cost) :-
  connected_to(A, B, DoorState),
  cost(DoorState, Cost).

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

reverse_path_cost([A,B],Cost) :-
  cost_of(B,A,Cost).
reverse_path_cost([A,B|T],Cost) :-
  cost_of(B,A,Cost1),
  reverse_path_cost([B|T],Cost2),
  Cost is Cost1+Cost2.

extend([Node|Path],NewPaths) :-
  findall([NewNode,Node|Path],
    (connected_to(Node,NewNode,_), 
    \+ member(NewNode,Path)),
    NewPaths).
