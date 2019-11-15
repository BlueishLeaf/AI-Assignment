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
  !,
  retract(item(Object, Location)),
  retract(robot(L,P,I)),
  assert(robot(L,P,[Object|I])).

unlock_door(A, B) :-
  robot(A, _, _),
  connected_to(A, B),
  !,
  has_locked_door(A, B),
  retract(door(A, B, locked)),
  retract(robot(L, P, I)),
  assert(robot(L, P - 2, I)),
  assert(door(A, B, closed)).

open_door(A, B) :-
  robot(A, _, _),
  connected_to(A, B),
  !,
  has_closed_door(A, B),
  retract(door(A, B, closed)),
  assert(door(A, B, open)).

move(A, B) :-
  can_move(A, B),
  retract(robot(_, P, H)),
  assert(robot(B, P, H)).

can_move(A, B) :-
  robot(A, _, _),
  connected_to(A, B),
  !,
  \+has_locked_door(A, B),
  \+has_closed_door(A, B).

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

connected(A, B) :- connection(A, B, _) ; connection(B, A, _).

% DEPTH FIRST SEARCH
% depth_first(A, B, P).
% A is the starting Node.
% B is the goal Node.
% P is the path from A to B.
depth_first(A, B, P) :-
    dfs(A, B, [A], Q),
   	reverse(Q, P).

dfs(A, B, P, [B|P]) :-
    connected(A, B).

dfs(A, B, V, P) :-
    connected(A, C),
    C \== B,
    \+ member(C, V),
    dfs(C, B, [C|V], P).

% BREADTH FIRST SEARCH
% breadth_first(A, B, P).
% A is the starting Node.
% B is the goal Node.
% P is the path from A to B.
breadth_first(A, B, P) :-
    bfs(B,[node(A,[])],[], Q),
    reverse(Q, P).

bfs(B, [node(B, P)|_], _, P).

% Ns is visited nodes
% Es is ???
% yeet the rich idk
bfs(B, [node(S, P1)|Ns], C, P) :-
    length(P1, L), % Length of the node path list
    findall(node(S1, [connected(S, S1)|P1]),
            (connected(S, S1),
            \+ (member(node(S1, P2), Ns), length(P2, L)),
            \+ member(S1, C)),
            Es),
    append(Ns, Es, O),
    bfs(B, O, [S|C], P). 

% writeln(L) is true if L is a list of items to be written on a line, followed by a newline.
writeln(L) :- writel(L),nl.

writel([]).
writel([H|T]) :- write(H), writel(T).
