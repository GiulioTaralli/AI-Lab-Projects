% -------------------------------------------------------
% A node is represented as: node(Position, G, H, F, Path)
% -------------------------------------------------------

% --------------------------------------------------------------------------------------
% A* Search Implementation
% - Start: start state
% - Check_Goal: predicate for retrieving the goal state or checking if a state is a goal
% - Check_Move: predicate for verifying if a generated state is valid or not
% - Heuristic: predicate for computing the specified heuristic 
% - Path: the output variable containing the chosen path
% --------------------------------------------------------------------------------------
% Entry point
a_star(Start, Check_Goal, Check_Move, Heuristic, Path) :-
    call(Heuristic, Start, H),
    F is H,
    a_star_search([node(Start, 0, H, F, [Start])], Check_Goal, Check_Move, Heuristic, Reverse_Path),
    reverse(Reverse_Path, Path),
    !.

a_star_search([node(Pos, _, _, _, Path)|_], Check_Goal, _, _, Path) :-
    call(Check_Goal, Pos).

a_star_search([node(Pos, G, _, _, Path)|Open], Check_Goal, Check_Move, Heuristic, Final_Path) :-
    findall(
        node(Next_Move, G_Next, H_Next, F_Next, [Next_Move|Path]),
        (
            call(Check_Move, Pos, Next_Move, Step_Cost),
            \+ member(Next_Move, Path),
            \+ member(node(Next_Move, _, _, _, _), Open),
            G_Next is G + Step_Cost,
            call(Heuristic, Next_Move, H_Next),
            F_Next is G_Next + H_Next
        ),
        Next_Moves
    ),
    append(Open, Next_Moves, Temp_Moves),
    sort_by_f(Temp_Moves, Moves),
    a_star_search(Moves, Check_Goal, Check_Move, Heuristic, Final_Path).

sort_by_f(Nodes, Sorted) :-
    map_list_to_pairs(get_f, Nodes, Pairs),
    keysort(Pairs, Sorted_Pairs),
    pairs_values(Sorted_Pairs, Sorted).

get_f(node(_, _, _, F, _), F).

% --------------------------------------------------------------------------------------
% IDA* Search Implementation
% - Start: start state
% - Check_Goal: predicate for retrieving the goal state or checking if a state is a goal
% - Check_Move: predicate for verifying if a generated state is valid or not
% - Heuristic: predicate for computing the specified heuristic 
% - Path: the output variable containing the chosen path
% --------------------------------------------------------------------------------------
% Entry point
ida_star(Start, Check_Goal, Check_Move, Heuristic, Path) :-
    call(Heuristic, Start, H),
    F is H,
    ida_loop(node(Start, 0, H, F, [Start]), F, Check_Goal, Check_Move, Heuristic, Rev_Path),
    reverse(Rev_Path, Path).

% Loop: update dynamic threshold
ida_loop(Node, Threshold, Check_Goal, Check_Move, Heuristic, Final_Path) :-
    ida_search(Node, Threshold, Check_Goal, Check_Move, Heuristic, Result),
    (
        Result = found(Final_Path) -> !   % Se la soluzione è trovata, fermati
    ;
        Result = threshold(New_Threshold),
        New_Threshold > Threshold,  % Se la nuova soglia è maggiore della precedente
        ida_loop(Node, New_Threshold, Check_Goal, Check_Move, Heuristic, Final_Path)
    ).

% Base case: goal found
ida_search(node(Pos, _, _, _, Path), _Threshold, Check_Goal, _, _, found(Path)) :-
    call(Check_Goal, Pos),
    !.

% Inductive case: recursion with threshold pruning
ida_search(node(Pos, G, _, _, Path), Threshold, Check_Goal, Check_Move, Heuristic, Result) :-
    call(Heuristic, Pos, H),
    F is G + H,
    ( F > Threshold ->
        Result = threshold(F), !  % Restituisci la soglia aumentata se non trovi soluzioni
    ;
        findall(
            Child_Result,
            (
                call(Check_Move, Pos, Next_Pos, Step_Cost),
                \+ member(Next_Pos, Path),  % Evita cicli
                G_Next is G + Step_Cost,
                call(Heuristic, Next_Pos, H_Next),
                F_Next is G_Next + H_Next,
                ida_search(node(Next_Pos, G_Next, H_Next, F_Next, [Next_Pos|Path]), 
                           Threshold, Check_Goal, Check_Move, Heuristic, Child_Result)
            ),
            Children_Results),
        process_results(Children_Results, Result)
    ).

% Analyze results
process_results(Results, found(Path)) :-
    member(found(Path), Results), 
    !.

process_results(Results, threshold(New_Threshold)) :-
    thresholds_from_results(Results, Thresholds),
    Thresholds \= [], % guard clause
    min_list(Thresholds, New_Threshold), 
    !.

% Transforms a list of constructs (found and threshold) into a list of values
thresholds_from_results([], []). % Base case

thresholds_from_results([threshold(T)|Tail], [T|Threshold_Values]) :- % Inductive case (head = threshold)
    thresholds_from_results(Tail, Threshold_Values).

thresholds_from_results([_|Tail], Threshold_Values) :- % Inductive case (head != threshold) (fallback)
    thresholds_from_results(Tail, Threshold_Values).