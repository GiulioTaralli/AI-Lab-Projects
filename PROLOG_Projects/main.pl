:- set_prolog_flag(verbose_load, silent). % It hides loading messages during consultation
:- set_prolog_flag(answer_write_options, [max_depth(0)]). % It allows to print the entire list without truncation

% Loading files with predicates for the labyrinth, puzzle and search strategies
:- [activity1_labyrinth].
:- [activity1_puzzle].
:- [search].

% Prints the length (or depth) of the given solution path
get_path_depth(Path) :-
    length(Path, Depth),
    format("Solution length: ~w~n", [Depth]).

% Solve labyrinth problem using A* algorithm
solve_labyrinth_a_star() :-
    start_state_labyrinth(Start),
    time(a_star(Start, goal_state_labyrinth, check_next_position, heuristic_labyrinth, Path)),
    get_path_depth(Path),
    print_path_labyrinth(Path).

% Solve labyrinth problem using IDA* algorithm
solve_labyrinth_ida_star() :-
    start_state_labyrinth(Start),
    time(ida_star(Start, goal_state_labyrinth, check_next_position, heuristic_labyrinth, Path)),
    get_path_depth(Path),
    print_path_labyrinth(Path).

% Solve puzzle problem using A* algorithm
solve_puzzle_a_star() :-
    start_state_puzzle(Start),
    time(a_star(Start, goal_check_puzzle, check_next_configuration, heuristic_puzzle, Path)),
    get_path_depth(Path),
    print_puzzle_grid(Path).

% Solve puzzle problem using IDA* algorithm
solve_puzzle_ida_star() :-
    start_state_puzzle(Start),
    time(ida_star(Start, goal_check_puzzle, check_next_configuration, heuristic_puzzle, Path)),
    get_path_depth(Path),
    print_puzzle_grid(Path).