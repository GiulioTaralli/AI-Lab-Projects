% ==== INCLUDE TEAM DATA ====
#include "teams.lp".

% ==== DOMAINS ====
% Explicit list of the 8 groups
group(g1; g2; g3; g4; g5; g6; g7; g8).

% 3 matchdays
matchdays(1..3).

% Pots (used only in the constraint)
pot(top_seed; pot_2; pot_3; underdog).

% ==== ASSIGNMENT OF TEAMS TO THE GROUPS ====

% Each team in exactly one group
{ assigned(T,G) : group(G) } = 1 :- team(T,_,_).

% Each group has exactly one team from each pot
:- group(G), pot(P), #count { T : team(T,_,P), assigned(T,G) } != 1.

% Which continents appear in a circle
cont_in(G,Cont) :- team(T,Cont,_), assigned(T,G).

% Each group must have at least 3 distinct continents
:- group(G), #count { C : cont_in(G,C) } < 3.

% ==== GAME GENERATION ====

% Each pair of teams from the same group plays exactly once, in one of the 3 matchdays
{ play(T1,T2,D,G) : matchdays(D) } = 1 :- assigned(T1,G), assigned(T2,G), T1 < T2, group(G).

% Normalize the match in both directions to count the games per team
play_norm(T1,T2,D,G) :- play(T1,T2,D,G).
play_norm(T2,T1,D,G) :- play(T1,T2,D,G).

% Each team plays exactly 1 match per matchday
:- team(T,_,_), group(G), matchdays(D), assigned(T,G), #count { T2 : play_norm(T,T2,D,G) } != 1.

% In each group, on each matchdays, there must be exactly 2 matches
:- group(G), matchdays(D), #count { T1,T2 : play(T1,T2,D,G) } != 2.

% ==== OUTPUT ====
% To assign teams by group
team_in_group(G, T) :- assigned(T, G).

% To order the matchdays
match_day(D, T1, T2, G) :- play(T1, T2, D, G).

#show team_in_group/2.
#show match_day/4.