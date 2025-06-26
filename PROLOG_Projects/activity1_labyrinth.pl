% ---------------------------
% Labyrinth domain definition
% ---------------------------
dim(20, 20).
start(1, 0).
goal(17, 19). 
goal(19, 19).

% Obstacles
block(2,1).
block(2,2).
block(2,3).
block(2,4).
block(2,5).
block(2,6).
block(2,7).
block(2,8).
block(2,9).
block(2,10).
block(2,11).
block(2,12).
block(2,13).
block(2,14).
block(2,15).
block(2,16).
block(2,18).
block(2,19).

block(3,16).
block(4,16).
block(4,17).
block(4,18).
block(5,18).
block(6,16).
block(6,17).
block(6,18).
block(7,16).
block(8,16).
block(8,18).
block(8,19).
block(9,16).
block(9,18).
block(10,16).
block(10,18).
block(10,19).
block(11,16).
block(12,16).
block(10,17).
block(10,18).
block(13,18).
block(14,17).
block(14,18).
block(15,17).
block(16,17).
block(16,19).
block(17,17).
block(18,17).
block(18,18).
block(18,19).

% Predicates that can be used for retrieve the start/goal position or check if a position is a start/goal
start_state_labyrinth((R, C)) :- start(R, C).
goal_state_labyrinth((R, C)) :- goal(R, C).

%------------------
% Moving predicates
%------------------

% Wrapper predicate for seach.pl
% - Current_Position: the current position
% - Next_Position: the next position to be evaluated
% - Cost: the movement cost (always 1)
check_next_position(Pos, Next, 1) :- check_move_labyrinth(Pos, Next).

% Valid movements avoiding blocks and staying in the grid
check_move_labyrinth((R_Old, C), (R_New, C)) :-  % up and down
    (R_New is R_Old + 1; R_New is R_Old - 1), 
    check_inside_grid(R_New, C), 
    \+ block(R_New, C).

check_move_labyrinth((R, C_Old), (R, C_New)) :-  % left and right
    (C_New is C_Old + 1; C_New is C_Old - 1), 
    check_inside_grid(R, C_New), 
    \+ block(R, C_New).

% Check that a position is inside the grid
check_inside_grid(R, C) :- dim(Max_R, Max_C), R >= 0, R < Max_R, C >= 0, C < Max_C.

% -----------------------------------------------------------------------
% Heuristic: Manhattan distance between the current position and the goal
% -----------------------------------------------------------------------
heuristic_labyrinth((Row, Column), H) :-
    findall(
        Distance, 
        (goal(Row_Goal, Column_Goal), Distance is abs(Row - Row_Goal) + abs(Column - Column_Goal)), 
        Distances
    ),
    min_list(Distances, H).

%------------------
% Printing the path
%------------------
positions_per_row(10).

print_path_labyrinth(Path) :-
    write("Start "),
    print_path_segments(Path),
    write(" Finish."),
    nl,
    !.

% Stampa il path a segmenti di N elementi per riga
print_path_segments(Path) :-
    positions_per_row(N),
    print_path_segments(Path, N).

print_path_segments([], _).

print_path_segments(Path, N) :-
    length(Prefix, N),
    append(Prefix, Rest, Path), !,
    print_path_line(Prefix),
    (Rest \= [] -> write('->'), nl ; true),
    print_path_segments(Rest, N).

print_path_segments(Rest, _) :-
    Rest \= [],
    print_path_line(Rest).

% Stampa una singola riga del path
print_path_line([]).

print_path_line([Step]) :-
    format(' (~w) ', [Step]).

print_path_line([Step1, Step2 | Rest]) :-
    format(' (~w) ->', [Step1]),
    print_path_line([Step2 | Rest]).
