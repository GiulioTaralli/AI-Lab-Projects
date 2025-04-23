% ---------------------------
% Labyrinth domain definition
% ---------------------------
dim(10, 10).
start(1, 0).
goal(8, 9).
goal(3, 9).

% Obstacles (each row represents a column)
block(1, 1). block(2, 1). block(3, 1). block(4, 1). block(5, 1). block(6, 1). block(7, 1). block(8, 1).
block(5, 2).
block(1, 3). block(4, 3). block(7, 3). block(8, 3).
block(1, 4). block(3, 4). block(6, 4). block(7, 4). block(8, 4).
block(1, 5). block(8, 5).
block(1, 6). block(3, 6). block(4, 6). block(5, 6). block(6, 6). block(8, 6).
block(8, 7).
block(1, 8). block(3, 8). block(4, 8). block(6, 8). block(7, 8). block(8, 8). block(9, 8).
block(4, 9).

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

% Print the path with arrows
print_path_labyrinth(Path) :-
    positions_per_row(N),
    print_path_segments(Path, 0, N), 
    !.

print_path_segments([], _).

print_path_segments(Path, Index, Max_Length) :-
    length(Prefix, Max_Length),
    append(Prefix, Rest, Path), !,
    write("Start "),
    (Index > 0 -> write(' -> ') ; true),
    print_path_line(Prefix),
    (Rest \= [] -> writeln(' ->') ; nl),
    NextIndex is Index + 1,
    print_path_segments(Rest, NextIndex, Max_Length),
    format(" Finish."),
    nl.

print_path_segments(Rest, Index, _) :-
    Rest \= [],
    (Index > 0 -> write('   -> ') ; true),
    print_path_line(Rest).

print_path_line([]).

print_path_line([Step]) :-
    format('(~w)', [Step]).

print_path_line([Step1, Step2 | Rest]) :-
    format('(~w) -> ', [Step1]),
    print_path_line([Step2 | Rest]).