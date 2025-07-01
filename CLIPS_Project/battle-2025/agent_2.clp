; ###################################### AGENT 2 ###############################################
(defmodule AGENT (import MAIN ?ALL) (import ENV ?ALL) (export ?ALL))

(deftemplate agent-k-per-row
  (slot row)
  (slot num)
)

(deftemplate agent-k-per-col
  (slot col)
  (slot num)
)

(defrule initialize-agent-k-row
  ?r <- (k-per-row (row ?row) (num ?num))
  =>
  (assert (agent-k-per-row (row ?row) (num ?num)))
)

(defrule initialize-agent-k-col
  ?c <- (k-per-col (col ?col) (num ?num))
  =>
  (assert (agent-k-per-col (col ?col) (num ?num)))
)

(deffacts agent-init-trigger
  (init-agent-k-counters)
)

(defrule trigger-initialize-agent-k
  ?f <- (init-agent-k-counters)
  =>
  (retract ?f)
  (assert (start-agent-k-init))
)

(defrule initialize-agent-k-row
  (start-agent-k-init)
  ?r <- (k-per-row (row ?row) (num ?num))
  =>
  (assert (agent-k-per-row (row ?row) (num ?num)))
)

(defrule initialize-agent-k-col
  (start-agent-k-init)
  ?c <- (k-per-col (col ?col) (num ?num))
  =>
  (assert (agent-k-per-col (col ?col) (num ?num)))
)

(deftemplate agent-cell
  (slot x)
  (slot y)
  (slot content (default unknown) ; default automatico
        (allowed-values unknown water top bot middle left right sub guessed))
)

(deffacts agent-init-cells
  (init-agent-grid)
)

(defrule initialize-agent-grid
  ?f <- (init-agent-grid)
  =>
  (retract ?f)
  (bind ?i 0)
  (while (< ?i 10) do
    (bind ?j 0)
    (while (< ?j 10) do
      (assert (agent-cell (x ?i) (y ?j) (content unknown)))
      (bind ?j (+ ?j 1))
    )
    (bind ?i (+ ?i 1))
  )
)

; Numero di navi ancora da scoprire per tipo 
(deftemplate submarine	; 4 sottomarini (1 casella)
	(slot to_find)
)
(deftemplate destroyer	; 3 cacciatorpediniere (2 caselle)
	(slot to_find)
)
(deftemplate cruiser	; 2 incrociatori (3 caselle)
	(slot to_find)
)
(deftemplate battleship	; 1 corazzata (4 caselle)
	(slot to_find)
)

(deffacts boat_to_find
	(submarine (to_find 4))
	(destroyer (to_find 3))
	(cruiser (to_find 2))
	(battleship (to_find 1))
)

;Template per controlli sulla cella
(deftemplate cell_status
	(slot kx)
	(slot ky)
	(slot stat (allowed-values guessed fired) )
)
; Template per controllare se e' gia' presa in considerazione per decremento
(deftemplate cell_dec  
	(slot dx)
	(slot dy)
	(slot stat_dec (allowed-values yes no)(default no))
)


; Template per controlli su righe/colonne (evitare ripetizioni)
(deftemplate k-row-water
	(slot row)
)
(deftemplate k-col-water
	(slot col)
)

; Template per tenere traccia degli incrociatori trovati
(deftemplate cruiser_vert_found
	(slot xtop)
	(slot xmid)
	(slot xbot)
	(slot y)
)
(deftemplate cruiser_orizz_found
	(slot x)
	(slot ysx)
	(slot ymid)
	(slot ydx)
)

; Template per tenere traccia dei cacciatorpedinieri trovati
(deftemplate destroyer_vert_found
	(slot xtop)
	(slot xbot)
	(slot y)
)
(deftemplate destroyer_orizz_found
	(slot x)
	(slot ysx)
	(slot ydx)
)

; Template per tenere traccia delle corazzate trovate
(deftemplate battleship_vert_found
	(slot xtop)
	(slot xmid)
	(slot xmid1)
	(slot xbot)
	(slot y)
)
(deftemplate battleship_orizz_found
	(slot x)
	(slot ysx)
	(slot ymid)
	(slot ymid1)
	(slot ydx)
)

; Indice sfruttato per fire su best row
(deftemplate indexFire
	(slot i)
)

(deffacts initIndex
	(indexFire (i 0))
)

;  ###################### INIZIALIZZAZIONE ########################

; Stampa in caso non siano presenti celle note all'inizio
(defrule beginning-no-knowledge (declare (salience 501))
	(not (k-cell (x ?x) (y ?y) (content ?t) ))
=>
	(printout t "No known cells at the beginning" crlf)
)

(defrule initializeWaterAgentRow (declare (salience 500))
   (status (step ?s) (currently running))
   (agent-k-per-row (row ?r) (num 0))
   (not (k-row-water (row ?r)))
   =>
   (printout t "AGENT: deduced WATER on ROW " ?r crlf)
    ;; Loop over the col in this row
    (bind ?j 0)
    (while (< ?j 10) do
     (do-for-fact ((?f agent-cell))
       (and (eq ?f:x ?r)
            (eq ?f:y ?j)
            (eq ?f:content unknown))
       (modify ?f (content water))
     )
     (bind ?j (+ ?j 1))
    )
   ;; Mark that we've processed this row
   (assert (k-row-water (row ?r)))
   (printout t crlf)
)

(defrule initializeWaterAgentCol (declare (salience 500))
   (status (step ?s) (currently running))
   (agent-k-per-col (col ?c) (num 0))
   (not (k-col-water (col ?c)))
   =>
   (printout t "AGENT: deduced WATER on COL " ?c crlf)
    ;; Loop over the row in this column
    (bind ?i 0)
    (while (< ?i 10) do
     (do-for-fact ((?f agent-cell))
       (and (eq ?f:y ?c)
            (eq ?f:x ?i)
            (eq ?f:content unknown)) ;
       (modify ?f (content water))
     )
     (bind ?i (+ ?i 1))
    )
   ;; Mark that we've processed this row
   (assert (k-col-water (col ?c)))
   (printout t crlf)
)

; Asserisce acqua in tutte le celle che contornano la tabella di gioco per evitare problemi di socnfinamento
(defrule settWaterBoundary (declare (salience 500))
	(status (step ?s)(currently running))
=>
	(assert (agent-cell (x -1) (y 0)))	(assert (agent-cell (x -1) (y 1)))	(assert (agent-cell (x -1) (y 2)))
	(assert (agent-cell (x -1) (y 3)))	(assert (agent-cell (x -1) (y 4)))	(assert (agent-cell (x -1) (y 5)))
	(assert (agent-cell (x -1) (y 6)))	(assert (agent-cell (x -1) (y 7)))	(assert (agent-cell (x -1) (y 8)))
	(assert (agent-cell (x -1) (y 9)))

	(assert (agent-cell (x 0) (y -1)))	(assert (agent-cell (x 1) (y -1)))	(assert (agent-cell (x 2) (y -1)))
	(assert (agent-cell (x 3) (y -1)))	(assert (agent-cell (x 4) (y -1)))	(assert (agent-cell (x 5) (y -1)))
	(assert (agent-cell (x 6) (y -1)))	(assert (agent-cell (x 7) (y -1)))	(assert (agent-cell (x 8) (y -1)))
	(assert (agent-cell (x 9) (y -1)))

	(assert (agent-cell (x 0) (y 10)))	(assert (agent-cell (x 1) (y 10)))	(assert (agent-cell (x 2) (y 10)))
	(assert (agent-cell (x 3) (y 10)))	(assert (agent-cell (x 4) (y 10)))	(assert (agent-cell (x 5) (y 10)))
	(assert (agent-cell (x 6) (y 10)))	(assert (agent-cell (x 7) (y 10)))	(assert (agent-cell (x 8) (y 10)))
	(assert (agent-cell (x 9) (y 10)))

	(assert (agent-cell (x 10) (y 0)))	(assert (agent-cell (x 10) (y 1)))	(assert (agent-cell (x 10) (y 2)))
	(assert (agent-cell (x 10) (y 3)))	(assert (agent-cell (x 10) (y 4)))	(assert (agent-cell (x 10) (y 5)))
	(assert (agent-cell (x 10) (y 6)))	(assert (agent-cell (x 10) (y 7)))	(assert (agent-cell (x 10) (y 8)))
	(assert (agent-cell (x 10) (y 9)))
)

; ############## GESTIONE DOPO GUESS ###############
; Viene decrementato il numero riga e colonna dopo guess su una cella   
(defrule dec_row_after_guess
	(status (step ?s)(currently running))
	(cell_status (kx ?r) (ky ?c) (stat guessed) )
	(not (cell_dec (dx ?r)(dy ?c)(stat_dec yes)) )

    ?numr <- (agent-k-per-row (row ?r) (num ?nr&:(> ?nr 0)))
    ?numc <- (agent-k-per-col (col ?c) (num ?nc&:(> ?nc 0)))
    
    ?cell <- (agent-cell (x ?r) (y ?c) (content ?content))
=>
    (modify ?numr (num (- ?nr 1)))
    (modify ?numc (num (- ?nc 1)))
    (modify ?cell (x ?r) (y ?c) (content guessed))
	(assert (cell_dec (dx ?r)(dy ?c)(stat_dec yes)) )
    (printout t "AGENT: decremented k-counter at row " ?r " and col " ?c crlf)
	(printout t crlf)
)

; ################ GESTIONE DELLE NAVI TROVATE ##########################

; Incrociatori  verticali
(defrule verticalCruiserFound 
	(status (step ?s)(currently running))
	?ctf <- (cruiser (to_find ?to_find_c ))
	(cruiser (to_find ?to_find_c &:(> ?to_find_c 0)))
	
	(or			; Se middle e' guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed) ) 
		(cell_status (kx ?x) (ky ?y) (stat fired) ) 
	)
	(or			; Se top e' guessed or fired
		(cell_status (kx ?x_top &:(eq ?x_top (- ?x 1))) (ky ?y) (stat guessed) )
		(cell_status (kx ?x_top &:(eq ?x_top (- ?x 1))) (ky ?y) (stat fired) )
	)	
	(or			; Se bot e' guessed or fired
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 1))) (ky ?y) (stat guessed) )	
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 1))) (ky ?y) (stat fired) )
	)
	; Controlla di avere water alle estremita'
	(k-cell (x ?x_top2 &:(eq ?x_top2 (- ?x 2))) (y ?y) (content water)) 
	(k-cell (x ?x_bot2 &:(eq ?x_bot2 (+ ?x 2))) (y ?y) (content water)) 
	; Controlla che non sia gia' stata trovata
	(not (cruiser_vert_found 
		(xtop ?xtop &:(eq ?xtop (- ?x 1)))
		(xmid ?x)
		(xbot ?xbot &:(eq ?xbot (+ ?x 1))) 
		(y ?y)
		))
=>	
	(modify ?ctf (to_find (- ?to_find_c 1)))
	(assert (cruiser_vert_found (xtop (- ?x 1))	(xmid ?x) (xbot (+ ?x 1)) (y ?y) ))
	(printout t crlf)
	(printout t "VERTICAL CRUISER FOUND!!")
	(printout t crlf)
)

; Incrociatori  orizzontali
(defrule horizontalCruiserFound
	(status (step ?s)(currently running))
	?ctf <- (cruiser (to_find ?to_find_c ))
	(cruiser (to_find ?to_find_c &:(> ?to_find_c 0)))
	(or			; Se middle e' guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed) ) 
		(cell_status (kx ?x) (ky ?y) (stat fired) ) 
	)
	(or			; Se top e' guessed or fired
		(cell_status (kx ?x) (ky ?y_left &:(eq ?y_left (- ?y 1))) (stat guessed) )
		(cell_status (kx ?x) (ky ?y_left &:(eq ?y_left (- ?y 1))) (stat fired) )
	)	
	(or			; Se right e' guessed or fired
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 1))) (stat guessed) )	
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 1))) (stat fired) )
	)
	; Controlla di avere water alle estremita'
	(k-cell (x ?x) (y ?y_left2 &:(eq ?y_left2 (- ?y 2))) (content water)) 
	(k-cell (x ?x) (y ?y_right2 &:(eq ?y_right2 (+ ?y 2))) (content water)) 
	; Controlla che non sia gia' stata trovata
	(not (cruiser_orizz_found 
		 (x ?x) (ysx ?yleft &:(eq ?yleft (- ?y 1))) 
		 (ymid ?y) 
		 (ydx ?yright &:(eq ?yright (+ ?y 1))) 
	)) 
=>	
	(modify ?ctf (to_find (- ?to_find_c 1)))
	(assert (cruiser_orizz_found (x ?x) (ysx (- ?y 1)) (ymid ?y) (ydx (+ ?y 1)) )) 
	(printout t crlf)
	(printout t "HORIZONTAL CRUISER FOUND!!")
	(printout t crlf)
)

; Cacciatorpedinieri verticali
(defrule verticalDestroyerFound
	(status (step ?s)(currently running))
	?ctf <- (destroyer (to_find ?to_find_c ))
	(destroyer (to_find ?to_find_c &:(> ?to_find_c 0)))
	
	(or			; Se top e' guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed) ) 
		(cell_status (kx ?x) (ky ?y) (stat fired) ) 
	)
	(or			; Se bot e' guessed or fired
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 1))) (ky ?y) (stat guessed) )	
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 1))) (ky ?y) (stat fired) )
	)
	; Controlla di avere water alle estremita'
	(k-cell (x ?x_top2 &:(eq ?x_top2 (- ?x 1))) (y ?y) (content water)) 
	(k-cell (x ?x_bot2 &:(eq ?x_bot2 (+ ?x 2))) (y ?y) (content water)) 
	; Controlla che non sia gia' stata trovata
	(not (destroyer_vert_found (xtop ?x) (xbot ?xbot &:(eq ?xbot (+ ?x 1))) (y ?y) ))
=>	
	(modify ?ctf (to_find (- ?to_find_c 1)))
	(assert (destroyer_vert_found (xtop  ?x ) (xbot (+ ?x 1)) (y ?y)))
	(printout t crlf)
	(printout t "VERTICAL DESTROYER FOUND!!")
	(printout t crlf)
)

; Cacciatorpedinieri orizzontali
(defrule horizontalDestroyerFound 
	(status (step ?s)(currently running))
	?ctf <- (destroyer (to_find ?to_find_c ))
	(destroyer (to_find ?to_find_c &:(> ?to_find_c 0)))
	(or			; Se left e' guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed) ) 
		(cell_status (kx ?x) (ky ?y) (stat fired) ) 
	)
	(or			; Se right e' guessed or fired
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 1))) (stat guessed) )	
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 1))) (stat fired) )
	)
	; Controlla di avere water alle estremita'
	(k-cell (x ?x) (y ?y_left2 &:(eq ?y_left2 (- ?y 1))) (content water)) 
	(k-cell (x ?x) (y ?y_right2 &:(eq ?y_right2 (+ ?y 2))) (content water)) 
	; Controlla che non sia gia' stata trovata
	(not (destroyer_orizz_found (x ?x) (ysx ?y) (ydx ?yright &:(eq ?yright (+ ?y 1))) )) 
=>	
	(modify ?ctf (to_find (- ?to_find_c 1)))
	(assert (destroyer_orizz_found (x ?x) (ysx  ?y ) (ydx (+ ?y 1)) )) 
	(printout t crlf)
	(printout t "HORIZONTAL DESTROYER FOUND!!")
	(printout t crlf)
)

; Corazzate verticali
(defrule verticalBattleshipFound
	(status (step ?s)(currently running))
	?btf <- (battleship (to_find ?to_find_b ))
	(battleship (to_find ?to_find_b &:(> ?to_find_b 0)))
	
	(or			; Se middle e' guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed) ) 
		(cell_status (kx ?x) (ky ?y) (stat fired) ) 
	)
	(or			; Se top e' guessed or fired
		(cell_status (kx ?x_top &:(eq ?x_top (- ?x 1))) (ky ?y) (stat guessed) )
		(cell_status (kx ?x_top &:(eq ?x_top (- ?x 1))) (ky ?y) (stat fired) )
	)	
	(or			; Se middle1 e' guessed or fired
		(cell_status (kx ?x_mid &:(eq ?x_mid (+ ?x 1))) (ky ?y) (stat guessed) )	
		(cell_status (kx ?x_mid &:(eq ?x_mid (+ ?x 1))) (ky ?y) (stat fired) )
	)
	(or			; Se bot e' guessed or fired
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 2))) (ky ?y) (stat guessed) )	
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 2))) (ky ?y) (stat fired) )
	)
	; Controlla di avere water alle estremita'
	(k-cell (x ?x_top2 &:(eq ?x_top2 (- ?x 2))) (y ?y) (content water)) 
	(k-cell (x ?x_bot2 &:(eq ?x_bot2 (+ ?x 3))) (y ?y) (content water)) 
	; Controlla che non sia gia' stata trovata
	(not (battleship_vert_found 
		(xtop ?xtop &:(eq ?xtop (- ?x 1)))
		(xmid ?x)
		(xmid1 ?xmid &:(eq ?xmid (+ ?x 1))) 
		(xbot ?xbot &:(eq ?xbot (+ ?x 2))) 
		))
=>	
	(modify ?btf (to_find (- ?to_find_b 1)))
	(assert (battleship_vert_found (xtop (- ?x 1))	(xmid ?x) (xmid1 (+ ?x 1)) (xbot (+ ?x 2)) ))
	(printout t crlf)
	(printout t "VERTICAL BATTLESHIP FOUND!!")
	(printout t crlf)
)

; Corazzate  orizzontali
(defrule horizontalBattleshipFound
	(status (step ?s)(currently running))
	?btf <- (battleship (to_find ?to_find_b ))
	(battleship (to_find ?to_find_b &:(> ?to_find_b 0)))
	(or			; Se middle e' guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed) ) 
		(cell_status (kx ?x) (ky ?y) (stat fired) ) 
	)
	(or			; Se top e' guessed or fired
		(cell_status (kx ?x) (ky ?y_left &:(eq ?y_left (- ?y 1))) (stat guessed) )
		(cell_status (kx ?x) (ky ?y_left &:(eq ?y_left (- ?y 1))) (stat fired) )
	)	
	(or			; Se middle1 e' guessed or fired
		(cell_status (kx ?x) (ky ?y_mid &:(eq ?y_mid (+ ?y 1))) (stat guessed) )	
		(cell_status (kx ?x) (ky ?y_mid &:(eq ?y_mid (+ ?y 1))) (stat fired) )
	)
	(or			; Se right e' guessed or fired
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 2))) (stat guessed) )	
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 2))) (stat fired) )
	)
	; Controlla di avere water alle estremita'
	(k-cell (x ?x) (y ?y_left2 &:(eq ?y_left2 (- ?y 2))) (content water)) 
	(k-cell (x ?x) (y ?y_right2 &:(eq ?y_right2 (+ ?y 3))) (content water)) 
	; Controlla che non sia gia' stata trovata
	(not (battleship_orizz_found 
		 (x ?x) (ysx ?yleft &:(eq ?yleft (- ?y 1))) 
		 (ymid ?y) 
		 (ymid1 ?ymid &:(eq ?ymid (+ ?y 1)))
		 (ydx ?yright &:(eq ?yright (+ ?y 2))) 
	)) 
=>	
	(modify ?btf (to_find (- ?to_find_b 1)))
	(assert (battleship_orizz_found (x ?x) (ysx (- ?y 1)) (ymid ?y) (ymid1 (+ ?y 1)) (ydx (+ ?y 2)) )) 
	(printout t crlf)
	(printout t "HORIZONTAL BATTLESHIP FOUND!!")
	(printout t crlf)
)

;  ####################### INVOCAZIONE MODULI ESTERNI #########################

; Quando non si puo' piu' fare niente il focus viene passato al modulo di esecuzione delle GUESS
(defrule guessModule 
	(status (step ?s)(currently running))
	(moves (guesses ?ng &:(> ?ng 0)))
	=>
	(printout t crlf)
	(focus GUESS_MOD)
)

; Quando non e' piu' possibile fare GUESS il focus viene passato al modulo di esecuzione delle FIRE
(defrule fireMod (declare (salience -300))
	(status (step ?s)(currently running))
	(moves (fires ?nf &:(> ?nf 0)))
	=>
	(printout t crlf)
	(focus FIRE_MOD)
    (focus FIRE_VAL)
)


;  ###################### OUTPUT DEBUG ########################
 
;(defrule print-agent-k-initialized
;  (agent-k-per-row (row ?r) (num ?n))
;  =>
;  (printout t "AGENT: Row " ?r " expects " ?n " boats." crlf)
;)

;(defrule print-agent-k-initialized-col
;  (agent-k-per-col (col ?c) (num ?n))
;  =>
;  (printout t "AGENT: Col " ?c " expects " ?n " boats." crlf)
;)

;(defrule print-initial-agent-cell (declare (salience -1000))
;  (agent-cell (x ?x) (y ?y) (content ?c))
;  =>
;  (printout t "AGENT CELL [ " ?x " , " ?y " ] : " ?c crlf)
;)

;  ####################### FINE #########################

(defrule solve (declare (salience -2000))
	(status (step ?s)(currently running))
	(submarine (to_find ?to_find_s))
	(destroyer (to_find ?to_find_d))
	(cruiser (to_find ?to_find_c))
	(battleship (to_find ?to_find_b))
	(moves (fires ?nf) (guesses ?ng) )
=>
	(assert (exec (step ?s) (action solve)))

	(printout t crlf)
	(printout t "I don't know what to do anymore: let's SOLVE!" crlf)
	(printout t crlf)
	(printout t "Fires left: " ?nf "   Guess left: " ?ng " "crlf)
	(pop-focus)
)