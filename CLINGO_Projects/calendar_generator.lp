% ==== INCLUDO I DATI DELLE SQUADRE ====
#include "teams.lp".

% ==== DOMINI ====
% elenco esplicito dei 8 gironi
group(g1; g2; g3; g4; g5; g6; g7; g8).

% 3 giornate
giornata(1..3).

% fasce (usate solo nel vincolo)
pot(top_seed; pot_2; pot_3; underdog).

% ==== ASSEGNAZIONE SQUADRE AI GIRONI ====

% Ogni squadra in esattamente un girone
{ assigned(T,G) : group(G) } = 1 :- team(T,_,_).

% Ogni girone ha esattamente una squadra di ciascuna fascia (pot)
:- group(G), pot(P),
   #count { T : team(T,_,P), assigned(T,G) } != 1.

% Quali continenti compaiono in un girone
cont_in(G,Cont) :- team(T,Cont,_), assigned(T,G).

% Ogni girone deve avere almeno 3 continenti distinti
:- group(G), #count { C : cont_in(G,C) } < 3.

% ==== GENERAZIONE PARTITE ====

% Ogni coppia di squadre dello stesso girone gioca esattamente una volta,
% in una delle 3 giornate
{ play(T1,T2,D,G) : giornata(D) } = 1
  :- assigned(T1,G), assigned(T2,G), T1 < T2, group(G).

% Normalizzo il match in entrambe le direzioni per contare le partite per squadra
play_norm(T1,T2,D,G) :- play(T1,T2,D,G).
play_norm(T2,T1,D,G) :- play(T1,T2,D,G).

% Ogni squadra gioca esattamente 1 partita per giornata
:- team(T,_,_), group(G), giornata(D), assigned(T,G),
   #count { T2 : play_norm(T,T2,D,G) } != 1.

% In ogni girone, in ogni giornata, ci devono essere esattamente 2 partite
:- group(G), giornata(D),
   #count { T1,T2 : play(T1,T2,D,G) } != 2.

% ==== OUTPUT ====
% Per assegnare le squadre per girone
team_in_group(G, T) :- assigned(T, G).

% Per ordinare le giornate
match_day(D, T1, T2, G) :- play(T1, T2, D, G).

#show team_in_group/2.
#show match_day/4.