% ==== INCLUDO I DATI DEI CORSI ====
#include "courses.lp".

% —— VINCOLI STRUTTURALI E ORARI ——

% Giorni validi per settimana
valid_day(W,D) :- week(W), fulltime_week(W), day(D).
valid_day(W,fri) :- week(W), not fulltime_week(W).
valid_day(W,sat) :- week(W), not fulltime_week(W).

% Ore valide per giorno
valid_hour(W,D,H) :- valid_day(W,D), D != sat, hour(H).
valid_hour(W,sat,H) :- valid_day(W,sat), hour_saturday(H).

% Ogni lezione può avvenire in una posizione valida del calendario
{ lesson(C,W,D,H) : valid_hour(W,D,H) } = Hours :- course(C,_, Hours).

% Evitare sovrapposizioni
:- lesson(C1,W,D,H), lesson(C2,W,D,H), C1 < C2.

% —— VINCOLO 1: Un docente non può tenere >4 ore in uno stesso giorno
:- professor(P), week(W), day(D), #count { C,H : lesson(C,W,D,H), course(C,P,_) } > 4.

% —— VINCOLO 2: A ciascun insegnamento vengono assegnate 2, 3 o 4 ore nello stesso giorno
ore_giorno(C,W,D,Count) :- course(C,_,_), valid_day(W,D), Count = #count { H : lesson(C,W,D,H) }.
:- ore_giorno(C,W,D,Count), Count > 0, Count < 2. % lezione da una sola ora -> scartare
:- ore_giorno(C,W,D,Count), Count > 4. % lezione da più di 4 ore -> scartare

% —— VINCOLO 3: Il primo giorno di lezione c'è la presentazione da 2 ore, ergo nessuna lezione deve sovrapporsi 
%               alla presentazione
presentation(1, fri, 1).
presentation(1, fri, 2).

:- lesson(_, 1, fri, H), H == 1.
:- lesson(_, 1, fri, H), H == 2.

% —— VINCOLO 4: Il calendario deve prevedere almeno 6 blocchi liberi di 2 ore ciascuno per eventuali recuperi 
%               di lezioni annullate o rinviate
free_spot(W,D,H) :-
    valid_hour(W,D,H),
    not lesson(_,W,D,H),
    not presentation(W,D,H).

free_block(W,D,H) :-
    free_spot(W,D,H),
    free_spot(W,D,H+1),
    valid_hour(W,D,H),
    valid_hour(W,D,H+1).

% Evitiamo sovrapposizioni
:- free_block(W,D,H), free_block(W,D,H+1).
:- free_block(W,D,H), free_block(W,D,H-1).

:- #count { W,D,H : free_block(W,D,H) } < 6.

% —— VINCOLO 5: L’insegnamento “Project Management” deve concludersi non oltre la prima settimana full-time
project_management_week(W) :- lesson("project_management", W, _, _).
project_management_last_week(W) :- W = #max { W1 : project_management_week(W1) }.
:- project_management_last_week(W), W > 7.

% —— VINCOLO 6: la prima lezione dell’insegnamento “Accessibilità e usabilità nella progettazione multimediale” 
%               deve essere collocata prima che siano terminate le lezioni dell’insegnamento “Linguaggi di markup”
acc_usab_prog_mult_lesson(W,D_NUM,H) :- lesson("accessibilita_usabilita_nella_progettazione_multimediale", W,D,H),
                                    day_num(D,D_NUM).
linguaggi_markup_lesson(W,D_NUM,H) :- lesson("linguaggi_di_markup", W,D,H), day_num(D,D_NUM).

first_access(W,D_NUM,H) :- (W,D_NUM,H) = #min { W1,D_NUM1,H1 : acc_usab_prog_mult_lesson(W1,D_NUM1,H1) }.
last_markup(W,D_NUM,H) :- (W,D_NUM,H) = #max { W2,D_NUM2,H2 : linguaggi_markup_lesson(W2,D_NUM2,H2) }.

:- first_access(W1,D_NUM1,H1), last_markup(W2,D_NUM2,H2), W1 > W2.
:- first_access(W1,D_NUM1,H1), last_markup(W2,D_NUM2,H2), W1 == W2, D_NUM1 > D_NUM2.
:- first_access(W1,D_NUM1,H1), last_markup(W2,D_NUM2,H2), W1 == W2, D_NUM1 == D_NUM2, H1 >= H2.

% —— VINCOLO 7: la distanza tra la prima e l’ultima lezione di ciascun insegnamento non deve superare le 
%               8 settimane 
% Trova settimana iniziale e finale di ogni corso
first_week(C,W,D,H) :- course(C,_,_), (W,N,H) = #min { (W1,N1,H1) : lesson(C,W1,D1,H1), day_num(D1,N1) },
                       day_num(D,N).
last_week(C,W,D,H) :- course(C,_,_), (W,N,H) = #max { (W2,N2,H2) : lesson(C,W2,D2,H2), day_num(D2,N2) },
                      day_num(D,N).

% vincolo: il numero di settimane nell’intervallo [W1..W2] non può essere > 9
:- first_week(C,W1,_,_), last_week(C,W2,_,_),
   #count { W : week(W), W1 <= W, W <= W2 } > 9.
   %conta quante di quelle settimane cadono nell’intervallo da W1 a W2

%:- first_week(C,W1,_,_), last_week(C,W2,_,_), week(W1), week(W2), W2 - W1 > 8.

span_info(C, W1, W2, N) :- 
    first_week(C,W1,_,_), 
    last_week(C,W2,_,_), 
    N = #count { W : week(W), W1 <= W, W <= W2 }.

% —— VINCOLO 8: le ore dell’insegnamento di “Tecnologie server-side per il web” devono essere organizzate 
%               in 5 blocchi da 4 ore ciascuno
target_course("tecnologie_server_side_per_il_web") :- course("tecnologie_server_side_per_il_web", _, _).

% Definisci blocchi validi da 4 ore consecutive
block4(C,W,D,H) :- 
    target_course(C),
    lesson(C,W,D,H),
    lesson(C,W,D,H+1),
    lesson(C,W,D,H+2),
    lesson(C,W,D,H+3),
    valid_hour(W,D,H),
    valid_hour(W,D,H+1),
    valid_hour(W,D,H+2),
    valid_hour(W,D,H+3).
% Vietare blocchi sovrapposti per sicurezza (facoltativo ma consigliato)
:- block4(C,W,D,H), block4(C,W,D,H2), H2 != H, H2 >= H, H2 < H+4.

% Conta i blocchi da 4 ore
num_block4(C, N) :- target_course(C), N = #count { W,D,H : block4(C,W,D,H) }.

% VINCOLO FINALE: Devono esserci esattamente 5 blocchi
:- num_block4(C,N), N != 5.

% —— VINCOLO 9: le prime lezioni degli insegnamenti “Crossmedia: articolazione delle scritture multimediali” 
%               e “Introduzione al social media management” devono essere collocate nella seconda settimana 
%               full-time
first_week_crossmedia(W1) :- course("crossmedia_articolazione_delle_scritture_multimediali",_,_), 
                            W1 = #min { W : lesson("crossmedia_articolazione_delle_scritture_multimediali",W,_,_) }.
first_week_social_media(W2) :- course("introduzione_al_social_media_management",_,_), 
                            W2 = #min { W : lesson("introduzione_al_social_media_management",W,_,_) }.
:- first_week_crossmedia(W1), W1 != 16.
:- first_week_social_media(W2), W2 != 16.

% —— VINCOLO 10: vincoli di propedeuticità
% Lezione di C2 non può iniziare prima della fine di C1
% Caso 1: C2 inizia in settimana precedente a quando finisce C1
:- prereq(C1, C2), last_week(C1, W1, _, _), first_week(C2, W2, _, _), W1 > W2.

% Caso 2: stessa settimana, giorno di C2 precedente
:- prereq(C1, C2), last_week(C1, W1, D1, _), first_week(C2, W2, D2, _), day_num(D1, N1), day_num(D2, N2), 
   W1 == W2, N1 > N2.

% Caso 3: stesso giorno, ora di C2 precedente o uguale
:- prereq(C1, C2), last_week(C1, W1, D1, H1), first_week(C2, W2, D2, H2), day_num(D1, N1), day_num(D2, N2), 
   W1 == W2, N1 == N2, H1 >= H2.

lesson_with_teacher(C, W, D, H, T) :- lesson(C, W, D, H), course(C, T, _).

% print di output essenziali
#show presentation/3.
#show lesson_with_teacher/5.
#show free_block/3.

% print check vincoli
#show span_info/4.
#show first_week/4.
#show last_week/4.