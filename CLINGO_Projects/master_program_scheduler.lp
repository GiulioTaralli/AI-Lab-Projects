% ==== INCLUDO I DATI DEI CORSI ====
#include "courses.lp".

% —— VINCOLI STRUTTURALI E ORARI ——

valid_day(W,D) :- week(W), fulltime_week(W), day(D).
valid_day(W,fri) :- week(W), not fulltime_week(W).
valid_day(W,sat) :- week(W), not fulltime_week(W).

valid_hour(W,D,H) :- valid_day(W,D), D != sat, hour(H).
valid_hour(W,sat,H)  :- valid_day(W,sat),        hour_saturday(H).

% Ogni lezione è assegnata in una qualche posizione del calendario
{ lesson(C,W,D,H,L) : valid_hour(W,D,H), dur(L), not invalid_block(H,L,D) } :- 
    course(C,_,_).

invalid_block(H,L,D) :- dur(L), D != sat, valid_hour(_,D,H), H+L-1 > 8.
invalid_block(H,L,sat) :- dur(L), valid_hour(_,sat,H), H+L-1 > 6.

presentation(1, monday, 1, 2).

% —— VINCOLO 1: Un docente non può tenere >4 ore in uno stesso giorno
:- lesson(C1,W,D,_,L1), lesson(C2,W,D,_,L2),
    course(C1,Prof,_), course(C2,Prof,_), C1 != C2,
    S = L1 + L2, S > 4.

% —— VINCOLO 2: A ciascun insegnamento vengono assegnate 2, 3 o 4 ore nello stesso giorno
% Vincolo: ogni insegnamento ha 2, 3 o 4 ore nello stesso giorno
:- course(C,_,_), week(W), valid_day(W,D),
   not 2 <= #sum { L : lesson(C,W,D,_,L) } <= 4.

% —— VINCOLO 3: nessuna lezione deve sovrapporsi alla presentazione
:- lesson(_, 1, monday, H, _), H < 11.

#show lesson/5.
