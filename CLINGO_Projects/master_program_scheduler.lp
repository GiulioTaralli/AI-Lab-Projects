% ==== INCLUDE COURSE DATA ====
#include "courses.lp".

% ==== STRUCTURAL CONSTRAINTS AND TIMETABLES ====

% Valid days per week
valid_day(W,D) :- week(W), fulltime_week(W), day(D).
valid_day(W,fri) :- week(W), not fulltime_week(W).
valid_day(W,sat) :- week(W), not fulltime_week(W).

% Valid hours per day
valid_hour(W,D,H) :- valid_day(W,D), D != sat, hour(H).
valid_hour(W,sat,H) :- valid_day(W,sat), hour_saturday(H).

% Each lesson can take place in a valid position on the calendar
{ lesson(C,W,D,H) : valid_hour(W,D,H) } = Hours :- course(C,_, Hours).

% Avoid overlaps
:- lesson(C1,W,D,H), lesson(C2,W,D,H), C1 < C2.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 1: A teacher cannot teach >4 hours in the same day
%--------------------------------------------------------------------------------------------------------------------
:- professor(P), week(W), day(D), #count { C,H : lesson(C,W,D,H), course(C,P,_) } > 4.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 2: Each course is assigned 2, 3 or 4 hours on the same day
%--------------------------------------------------------------------------------------------------------------------
hour_counter(C,W,D,Count) :- course(C,_,_), valid_day(W,D), Count = #count { H : lesson(C,W,D,H) }.
:- hour_counter(C,W,D,Count), Count > 0, Count < 2. % One hour lesson -> discard
:- hour_counter(C,W,D,Count), Count > 4.            % Lesson more than 4 hours -> discard

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 3: On the first day of class there is a 2-hour presentation, therefore no class should overlap
%                  with the presentation
%--------------------------------------------------------------------------------------------------------------------
presentation(1, fri, 1).
presentation(1, fri, 2).

:- lesson(_, 1, fri, H), H == 1.
:- lesson(_, 1, fri, H), H == 2.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 4: The calendar must include at least 6 free blocks of 2 hours each for any recoveries of lessons
%                  canceled or postponed
%--------------------------------------------------------------------------------------------------------------------
free_spot(W,D,H) :-
    valid_hour(W,D,H),
    not lesson(_,W,D,H),
    not presentation(W,D,H).

free_block(W,D,H) :-
    free_spot(W,D,H),
    free_spot(W,D,H+1),
    valid_hour(W,D,H),
    valid_hour(W,D,H+1).

% Avoid overlaps
:- free_block(W,D,H), free_block(W,D,H+1).
:- free_block(W,D,H), free_block(W,D,H-1).

:- #count { W,D,H : free_block(W,D,H) } < 6.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 5: The “Project Management” course must be concluded no later than the first full-time week
%--------------------------------------------------------------------------------------------------------------------
project_management_week(W) :- lesson("project_management", W, _, _).
project_management_last_week(W) :- W = #max { W1 : project_management_week(W1) }.
:- project_management_last_week(W), W > 7.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 6: the first lesson of the course “Accessibilità e usabilità nella progettazione multimediale”  
%                  must be placed before the lessons of the course “Linguaggi di markup” have finished
%--------------------------------------------------------------------------------------------------------------------
acc_usab_prog_mult_lesson(W,D_NUM,H) :- lesson("accessibilita_usabilita_nella_progettazione_multimediale", W,D,H),
                                    day_num(D,D_NUM).
linguaggi_markup_lesson(W,D_NUM,H) :- lesson("linguaggi_di_markup", W,D,H), day_num(D,D_NUM).

first_access(W,D_NUM,H) :- (W,D_NUM,H) = #min { W1,D_NUM1,H1 : acc_usab_prog_mult_lesson(W1,D_NUM1,H1) }.
last_markup(W,D_NUM,H) :- (W,D_NUM,H) = #max { W2,D_NUM2,H2 : linguaggi_markup_lesson(W2,D_NUM2,H2) }.

:- first_access(W1,D_NUM1,H1), last_markup(W2,D_NUM2,H2), W1 > W2.
:- first_access(W1,D_NUM1,H1), last_markup(W2,D_NUM2,H2), W1 == W2, D_NUM1 > D_NUM2.
:- first_access(W1,D_NUM1,H1), last_markup(W2,D_NUM2,H2), W1 == W2, D_NUM1 == D_NUM2, H1 >= H2.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 7: the distance between the first and last lesson of each course must not exceed 8 weeks
%--------------------------------------------------------------------------------------------------------------------               
% Find the start and end week of each course
first_week(C,W,D,H) :- course(C,_,_), (W,N,H) = #min {(W1,N1,H1) : lesson(C,W1,D1,H1), day_num(D1,N1)},
                       day_num(D,N).
last_week(C,W,D,H) :- course(C,_,_), (W,N,H) = #max {(W2,N2,H2) : lesson(C,W2,D2,H2), day_num(D2,N2)},
                      day_num(D,N).

% constraint: the number of weeks in the interval [W1..W2] cannot be > 9
:- first_week(C,W1,_,_), last_week(C,W2,_,_),
   #count {W : week(W), W1 <= W, W <= W2} > 9.
   % count how many of those weeks fall in the range W1 to W2

% Another way is:
% :- first_week(C,W1,_,_), last_week(C,W2,_,_), week(W1), week(W2), W2 - W1 > 8.

span_info(C, W1, W2, N) :- 
    first_week(C,W1,_,_), 
    last_week(C,W2,_,_), 
    N = #count { W : week(W), W1 <= W, W <= W2 }.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 8: the hours of teaching “Tecnologie server-side per il web” must be organized into 5 blocks of
%                  4 hours each
%--------------------------------------------------------------------------------------------------------------------
target_course("tecnologie_server_side_per_il_web") :- course("tecnologie_server_side_per_il_web", _, _).

% Define blocks valid for 4 consecutive hours
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
% Prohibit overlapping blocks for safety
:- block4(C,W,D,H), block4(C,W,D,H2), H2 != H, H2 >= H, H2 < H+4.

% Count 4-hour blocks
num_block4(C, N) :- target_course(C), N = #count { W,D,H : block4(C,W,D,H) }.

% Final constraint: There must be exactly 5 blocks
:- num_block4(C,N), N != 5.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 9: the first lessons of the courses “Crossmedia: articolazione delle scritture multimediali” 
%                  and “Introduzione al social media management” must be placed in the second full-time week
%--------------------------------------------------------------------------------------------------------------------
first_week_crossmedia(W1) :- course("crossmedia_articolazione_delle_scritture_multimediali",_,_), 
                            W1 = #min { W : lesson("crossmedia_articolazione_delle_scritture_multimediali",W,_,_) }.
first_week_social_media(W2) :- course("introduzione_al_social_media_management",_,_), 
                            W2 = #min { W : lesson("introduzione_al_social_media_management",W,_,_) }.
:- first_week_crossmedia(W1), W1 != 16.
:- first_week_social_media(W2), W2 != 16.

%--------------------------------------------------------------------------------------------------------------------
% —— CONSTRAINT 10: constraints of propaedeutic nature
%--------------------------------------------------------------------------------------------------------------------
% C2 lesson cannot start before the end of C1
% Case 1: C2 starts the week before C1 ends
:- prereq(C1, C2), last_week(C1, W1, _, _), first_week(C2, W2, _, _), W1 > W2.

% Case 2: same week, day of previous C2
:- prereq(C1, C2), last_week(C1, W, D1, _), first_week(C2, W, D2, _), day_num(D1, N1), day_num(D2, N2), 
   N1 > N2.

% Case 3: same week and day, previous or same C2 time
:- prereq(C1, C2), last_week(C1, W, D, H1), first_week(C2, W, D, H2), H1 >= H2.

lesson_with_teacher(C, W, D, H, T) :- lesson(C, W, D, H), course(C, T, _).

% ==== OUTPUT ====

% Essential output prints
#show presentation/3.
#show lesson_with_teacher/5.
#show free_block/3.

% print some important constraints to check the final result
#show span_info/4.
#show first_week/4.
#show last_week/4.