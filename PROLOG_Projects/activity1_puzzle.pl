% ------------------------
% Puzzle domain definition
% ------------------------

start_state_puzzle([7, 3, 1,
                    5, 0, 6,
                    8, 2, 4]).
goal_state_puzzle([1, 2, 3,
                   4, 5, 6,
                   7, 8, 0]).

% Index adjacencies in the 3x3 grid
neighbour(0, 1). neighbour(0, 3).
neighbour(1, 0). neighbour(1, 2). neighbour(1, 4).
neighbour(2, 1). neighbour(2, 5).
neighbour(3, 0). neighbour(3, 4). neighbour(3, 6).
neighbour(4, 1). neighbour(4, 3). neighbour(4, 5). neighbour(4, 7).
neighbour(5, 2). neighbour(5, 4). neighbour(5, 8).
neighbour(6, 3). neighbour(6, 7).
neighbour(7, 4). neighbour(7, 6). neighbour(7, 8).
neighbour(8, 5). neighbour(8, 7).

% Predicates that can be used for retrieve the strat combination or check if a combination is the start
goal_check_puzzle(State) :- goal_state_puzzle(State).

% ------------------------------
% Moving and swapping predicates
% ------------------------------

% Wrapper predicate for seach.pl
% - State: the current combination
% - Next_State: the next combination to be evaluated
% - Cost: the movement cost (always 1)
check_next_configuration(State, Next_State, 1) :- check_move_puzzle(State, Next_State).

% Check if the next combination is valid starting from the current one, if yes then swap the 0 with the involved number
check_move_puzzle(State, New_State) :-
    nth0(Zero_Index, State, 0),
    move(Zero_Index, Swap_Index),
    swap(State, Zero_Index, Swap_Index, New_State).

move(Zero_Index, Swap_Index) :-
    neighbour(Zero_Index, Swap_Index).

swap(List, Index1, Index2, Swapped) :-
    nth0(Index1, List, Elem1),
    nth0(Index2, List, Elem2),
    set_elem(List, Index1, Elem2, Temp),
    set_elem(Temp, Index2, Elem1, Swapped).

set_elem([_|Tail], 0, Elem, [Elem|Tail]).

set_elem([Head|Tail], Index, Elem, [Head|New_Tail]) :-
    Index > 0,
    Index1 is Index - 1,
    set_elem(Tail, Index1, Elem, New_Tail).

% ------------------------------------------------------------------
% Heuristic: Manhattan distance
% - total distance of all tiles to be moved to their target position
% ------------------------------------------------------------------
heuristic_puzzle(State, H) :-
    goal_state_puzzle(Goal),
    manhattan_distance(State, Goal, H).

% Compute the Manhattan distance for each tile (excluding 0)
manhattan_distance(State, Goal, Total_Dist) :-
    findall(
        Distance, 
        (
            nth0(Index_State, State, Elem_State), % Scan the State list and get index and element for every position
            Elem_State \= 0,
            nth0(Index_Goal, Goal, Elem_State), % Get the index of the element in the Goal list
            get_row_col(Index_State, Row_State, Column_State),
            get_row_col(Index_Goal, Row_Goal, Column_Goal),
            Distance is abs(Row_State - Row_Goal) + abs(Column_State - Column_Goal)
        ), 
        Distances
    ),
    sum_list(Distances, Total_Dist).

% Convert list indices (0-8) to row/column coordinates
get_row_col(Index, Row, Column) :-
    Row is Index // 3,
    Column is Index mod 3.

%----------------------
% Printing the solution
%----------------------
matrices_per_row(8).

% Print the combinations with an arrow in the middle in case of continuation
print_puzzle_grid(Path) :-
    matrices_per_row(N),
    print_puzzle_grid(Path, N).

print_puzzle_grid([], _).

print_puzzle_grid(Path, N) :-
    take(N, Path, Blocks, Rest),
    maplist(list_to_matrix, Blocks, Matrices),
    % if Rest is not empty, set Continuation = true
    ( Rest \= [] -> Continuation = true ; Continuation = false ),
    print_matrices(Matrices, Continuation),
    nl,
    print_puzzle_grid(Rest, N),
    !.

take(0, L, [], L).
take(_, [], [], []).
take(N, [H|T], [H|Tail], Rest) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Tail, Rest).

list_to_matrix([A,B,C,D,E,F,G,H,I], [[A,B,C],[D,E,F],[G,H,I]]).

% Prints a block of matrices side by side, with an optional continuation arrow
print_matrices(Matrices, Continuation) :-
    % line 1: only spaces between matrices
    maplist(nth0(0), Matrices, R1),
    print_line(R1),
    nl,
    % line 2: internal arrows between matrices
    maplist(nth0(1), Matrices, R2),
    print_line_with_arrow(R2),
    ( Continuation -> write(' ->') ; true ),
    nl,
    % line 3: only spaces, and arrow at the end if Continuation = true
    maplist(nth0(2), Matrices, R3),
    print_line(R3),
    write("\n").

% Print a row of matrices without arrows, with fixed spaces
print_line([]).
print_line([[A,B,C]]) :-
    format('[~w,~w,~w]    ', [A,B,C]).
print_line([[A,B,C]|Rest]) :-
    format('[~w,~w,~w]    ', [A,B,C]),
    print_line(Rest).

% Print a row of matrices with internal arrows “->”
print_line_with_arrow([]).
print_line_with_arrow([[A,B,C]]) :-
    format('[~w,~w,~w]', [A,B,C]).
print_line_with_arrow([[A,B,C]|Rest]) :-
    format('[~w,~w,~w] -> ', [A,B,C]),
    print_line_with_arrow(Rest).