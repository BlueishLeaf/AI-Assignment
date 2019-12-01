% DYNAMIC PREDICATES
:- dynamic item/2.
:- dynamic robot/3.
:- dynamic connected_to/3.

% ENTRY POINT FOR FINDING SOLUTIONS
% To deliver coffee to r101, try "?- resolve_goals([deliver(coffee, r101)])."
% You can also give it a series of tasks, such as "?- resolve_goals([deliver(coffee, r101), deliver(key, lab3)])."
resolve_goals([]).
resolve_goals([Goal|Rest]) :-
  format("Attempting to resolve task: ~w \n", [Goal]),
  Goal,
  resolve_goals(Rest).

resolve_path([]).
resolve_path(Goal) :-
  format("Current goal is: ~w \n", [Goal]),
  robot(L, _, _),
  uni_cost([[L]], Goal, Path, ExploredNodes),
  reverse(Path, Reverse),
  format("Nodes explored for this search: ~a \n", [ExploredNodes]),
  format("Path to travel: ~w \n", [Reverse]),
  traverse(Reverse),
  format("Arrived at goal: ~a \n", [Goal]).

% ROBOT
% robot(L, P, [I]).
% L is the robot's location
% P is the robot's remaining power
% I is the robot's inventory
robot(c0, 150, []).

% Place an item located in the current room in the robot's inventory
pick_up(Item) :-
  robot(L, _, _),
  item(Item, L),
  format("Picking up ~a from ~a \n", [Item, L]),
  retract(item(Item, L)),
  retract(robot(L,P,I)),
  assert(robot(L,P,[Item|I])).

% Remove an item from the robot's inventory and place it in the current room
drop(Item) :-
  is_holding(Item),
  format("Dropping ~a from inventory \n", [Item]),
  retract(robot(L, P, I)),
  delete(I, Item, NewI),
  assert(item(Item, L)),
  assert(robot(L, P, NewI)).

% Check if robot has a certain item in their inventory
is_holding(Item) :-
  robot(_, _, I),
  member(Item, I).

% Change the door state to closed if the robot has the key
unlock_door(A, B) :-
  connected_to(A, B, locked),
  robot(A, P, _),
  P >= 2,
  is_holding(key),
  format("Unlocking door between ~a and ~a \n", [A, B]),
  retract(connected_to(A, B, locked)),
  retract(robot(L, P, I)),
  NewP is P - 2,
  assert(connected_to(A, B, closed)),
  assert(robot(L, NewP, I)).
unlock_door(A, B) :-
  connected_to(B, A, locked),
  robot(A, P, _),
  P >= 2,
  is_holding(key),
  format("Unlocking door between ~a and ~a \n", [A, B]),
  retract(connected_to(B, A, locked)),
  retract(robot(L, P, I)),
  NewP is P - 2,
  assert(connected_to(B, A, closed)),
  assert(robot(L, NewP, I)).

% Change the door state to open if the door is closed
open_door(A, B) :-
  robot(A, P, _),
  P >= 3,
  connected_to(A, B, closed),
  format("Opening door between ~a and ~a \n", [A, B]),
  retract(connected_to(A, B, closed)),
  retract(robot(L, P, I)),
  NewP is P - 3,
  assert(connected_to(A, B, open)),
  assert(robot(L, NewP, I)).
open_door(A, B) :-
  robot(A, P, _),
  P >= 3,
  connected_to(B, A, closed),
  format("Opening door between ~a and ~a \n", [A, B]),
  retract(connected_to(B, A, closed)),
  retract(robot(L, P, I)),
  NewP is P - 3,
  assert(connected_to(B, A, open)),
  assert(robot(L, NewP, I)).

% Physically move the robot from one room to another
move(A, B) :-
  robot(A, P, _),
  P >= 1,
  connected_to(A, B, open),
  format("Moving from ~a to ~a \n", [A, B]),
  retract(robot(A, P, I)),
  NewP is P - 1,
  assert(robot(B, NewP, I)).
move(A, B) :-
  robot(A, Power, _),
  Power >= 1,
  connected_to(B, A, open),
  format("Moving from ~a to ~a \n", [A, B]),
  retract(robot(A, P, I)),
  NewP is P - 1,
  assert(robot(B, NewP, I)).

% Find and transport a desired item to a given location
deliver(Item, Location) :-
  item(Item, ItemLocation),
  key_required(Location),
  format("The delivery destination is locked. The key is required. \n"),
  item(key, KeyLocation),
  resolve_path(KeyLocation),
  pick_up(key),
  resolve_path(ItemLocation),
  pick_up(Item),
  resolve_path(Location),
  drop(Item),
  format("The ~a was successfully delivered to ~a \n", [Item, Location]).
deliver(Item, Location) :-
  item(Item, ItemLocation),
  key_required(Location),
  format("The delivery destination is locked. The key is required. \n"),
  item(key, KeyLocation),
  resolve_path(KeyLocation),
  pick_up(key),
  resolve_path(ItemLocation),
  pick_up(Item),
  resolve_path(Location),
  drop(Item),
  format("The ~a was successfully delivered to ~a \n", [Item, Location]).
deliver(Item, Location) :-
  item(Item, ItemLocation),
  format("Key is not required for this delivery. \n"),
  resolve_path(ItemLocation),
  pick_up(Item),
  resolve_path(Location),
  drop(Item),
  format("The ~a was successfully delivered to ~a \n", [Item, Location]).
deliver(Item, Location) :-
  is_holding(Item),
  key_required(Location),
  format("The ~a is already in the inventory. \n", [Item]),
  format("The delivery destination is locked. The key is required. \n"),
  item(key, KeyLocation),
  resolve_path(KeyLocation),
  pick_up(key),
  resolve_path(Location),
  drop(Item),
  format("The ~a was successfully delivered to ~a \n", [Item, Location]).
deliver(Item, Location) :-
  is_holding(Item),
  format("The ~a is already in the inventory. \n", [Item]),
  resolve_path(Location),
  drop(Item),
  format("The ~a was successfully delivered to ~a \n", [Item, Location]).

% Used to check if the goal node requires a key
key_required(Goal) :-
  connection(_, Goal, locked),
  \+ is_holding(key).

% Traverse a given path one node at a time
traverse([]).
traverse([Node|Path]) :-
  robot(L, _, _),
  Node \== L,
  connection(L, Node, locked),
  unlock_door(L, Node),
  open_door(L, Node),
  move(L, Node),
  traverse(Path).
traverse([Node|Path]) :-
  robot(L, _, _),
  Node \== L,
  connection(L, Node, closed),
  open_door(L, Node),
  move(L, Node),
  traverse(Path).
traverse([Node|Path]) :-
  robot(L, _, _),
  Node \== L,
  connection(L, Node, open),
  move(L, Node),
  traverse(Path).
traverse([Node|Path]) :-
  robot(L, _, _),
  Node == L,
  traverse(Path).

% Costs of each node
cost(open, 1).
cost(closed, 4). % 3 to open plus 1 to move
cost(locked, 6). % 2 to unlock plus 3 to open plus 1 to move

cost_of(A, B, Cost) :-
  connected_to(A, B, DoorState),
  cost(DoorState, Cost).
cost_of(A, B, Cost) :-
  connected_to(B, A, DoorState),
  cost(DoorState, Cost).

% Knowledge representation
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
connection(A, B, DS) :- connected_to(A, B, DS).
connection(A, B, DS) :- connected_to(B, A, DS).

% Uniform cost search implementation
uni_cost([[Goal|Path]|_], Goal, [Goal|Path], 0).
uni_cost([Path|Queue], Goal, FinalPath, N) :- 
  extend(Path, NewPaths),
  append(Queue, NewPaths, Queue1), 
  sort_queue(Queue1, NewQueue),
  uni_cost(NewQueue, Goal, FinalPath, M),
  N is M + 1.

sort_queue(L, L2) :-
  swap(L, L1), !,
  sort_queue(L1, L2).
sort_queue(L, L).

swap([X, Y|T],[Y, X|T]) :-
  reverse_path_cost(X, CX),
  reverse_path_cost(Y, CY),
  CX > CY.
swap([X|T],[X|V]) :-
  swap(T, V).

reverse_path_cost([A, B], Cost) :-
  cost_of(A, B, Cost).
reverse_path_cost([A,B|T], Cost) :-
  cost_of(A, B, Cost1),
  reverse_path_cost([B|T], Cost2),
  Cost is Cost1 + Cost2.

% With the key, the search algorithm will consider all connections when building a path
extend([Node|Path], NewPaths) :-
  is_holding(key),
  findall([NewNode, Node|Path],
    (connection(Node, NewNode, _),
    \+ member(NewNode, Path)),
    NewPaths).

% Without the key, the search algorithm will only consider connections that are open or closed
extend([Node|Path], NewPaths) :-
  \+ is_holding(key),
  findall([NewNode, Node|Path],
    ((connection(Node, NewNode, open); connection(Node, NewNode, closed)),
    \+ member(NewNode, Path)),
    NewPaths).