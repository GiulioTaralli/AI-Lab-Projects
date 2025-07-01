;  ##################### MODULO GUESS ############################



(defmodule GUESS_MOD (import MAIN ?ALL) (import ENV ?ALL) (import AGENT ?ALL) (export ?ALL))

;  --------------------------- GUESS CELLE NOTE ------------------------------------------------------

; Effettua una GUESS su una K-CELL con contenuto TOP
(defrule guessTopKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content top))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "guessTopKCell - Step " ?s ":    GUESS cell [" ?x "," ?y "] top"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	;guess K-CELL TOP
	; Asserisco WATER attorno alla K-CELL TOP
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) 	;sopra
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	;sx
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))	;dx
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	;diag sopra sx
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	;diag sopra dx
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	;diag sotto sx
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	;diag sotto dx
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Effettua una GUESS su una K-CELL con contenuto BOTTOM
(defrule guessBottomKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content bot))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "guessBottomKCell - Step " ?s ":    GUESS cell [" ?x "," ?y "] bot"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	;guess K-CELL BOT
	; Asserisco WATER attorno alla K-CELL BOTTOM
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) 	;sotto
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	;sx
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))	;dx
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	;diag sopra sx
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	;diag sopra dx
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	;diag sotto sx
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	;diag sotto dx
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Effettua una GUESS su una K-CELL con contenuto LEFT
(defrule guessLeftKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content left))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "guessLeftKCell - Step " ?s ":    GUESS cell [" ?x "," ?y "] left"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	;guess K-CELL LEFT
	; Asserisco WATER attorno alla K-CELL LEFT
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) 	;sotto
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) 	;sopra
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	;sx
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	;diag sopra sx
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	;diag sopra dx
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	;diag sotto sx
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	;diag sotto dx
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Effettua una GUESS su una K-CELL con contenuto SUB
(defrule guessKCellSub (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content sub))
	(not (exec (action guess) (x ?x) (y ?y)))
	?stf <- (submarine (to_find ?to_find_s &:(> ?to_find_s 0)) ) ; per contare num di sottomarini da trovare
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "AAA guessKCellSub - Step " ?s ":    GUESS cell [" ?x "," ?y "] sub"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	;guess K-CELL sub
    (modify ?stf (to_find (- ?to_find_s 1))) ; decremento numero sottomarini da trovare
	(printout t crlf)
	(printout t "SUBMARINE FOUND!!")
	(printout t crlf)
	; Asserisco WATER attorno alla K-CELL SUB
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) 	;sotto
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) 	;sopra
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	;sx
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))	;dx
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	;diag sopra sx
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	;diag sopra dx
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	;diag sotto sx
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	;diag sotto dx
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Effettua una GUESS su una K-CELL con contenuto RIGHT
(defrule guessRightKCell (declare (salience 100))
	(status (step ?s)(currently running))
	(k-cell (x ?x) (y ?y) (content right))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=> 
	(printout t crlf)
	(printout t "guessRightKCell - Step " ?s ":    GUESS cell [" ?x "," ?y "] right"crlf)
	(assert (exec(step ?s) (action guess) (x ?x) (y ?y)))		; questa cella
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))       ; dx
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water)))       ; sotto
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water)))       ; sopra
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water))) ; sopra-dx
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water))) ; sopra-sx
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water))) ; sotto-dx
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water))) ; sotto-sx
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Effettua una GUESS su una K-CELL con contenuto MIDDLE
(defrule guessMiddleKCell (declare (salience 100))
	(status (step ?s)(currently running))
	(k-cell (x ?x) (y ?y) (content middle))
	(not (exec (action guess) (x ?x) (y ?Y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=> 
	(printout t crlf)
	(printout t "guessMiddleKCell - Step " ?s ":    GUESS cell [" ?x "," ?y "] middle"crlf)
	(assert (exec(step ?s) (action guess) (x ?x) (y ?y)))		; questa cella
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water))) ; sopra-dx
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water))) ; sopra-sx
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water))) ; sotto-dx
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water))) ; sotto-sx
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; ############## GUESS DELLE CELLE AFFIANCO A NOTE ###############

; Effettua una GUESS su una cella posizionata sotto una K-CELL con contenuto TOP
(defrule guessCellUnderTopKCell 
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content top))
	(not (exec (action guess) (x ?x-under &:(eq ?x-under(+ ?x 1))) (y ?y))) ; se non eseguita guess su cella sotto
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	; GUESS sulla cella sotto alla K-CELL con contenuto TOP
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" (+ ?x 1) "," ?y "]"crlf)
	(assert (exec (step ?s) (action guess) (x (+ ?x 1)) (y ?y)))
	(assert (k-cell (x (+ ?x 2)) (y (- ?y 1)) (content water)))	;diag sotto sx
	(assert (k-cell (x (+ ?x 2)) (y (+ ?y 1)) (content water)))	;diag sotto dx

	(assert (cell_status (kx (+ ?x 1)) (ky ?y) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Effettua una GUESS su una cella posizionata sopra una K-CELL con contenuto BOTTOM
(defrule guessCellOnTopBottomKCell
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content bot))
	(not (exec (action guess) (x ?x-top &:(eq ?x-top(- ?x 1))) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	; GUESS sulla cella sopra alla K-CELL con contenuto BOTTOM
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" (- ?x 1) "," ?y "]"crlf)
	(assert (exec (step ?s) (action guess) (x (- ?x 1)) (y ?y)))
	(assert (k-cell (x (- ?x 2)) (y (- ?y 1)) (content water)))	;diag sotto sx
	(assert (k-cell (x (- ?x 2)) (y (+ ?y 1)) (content water)))	;diag sotto dx

	(assert (cell_status (kx (- ?x 1)) (ky ?y) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Effettua una GUESS su una cella posizionata a destra di una K-CELL con contenuto LEFT
(defrule guessCellOnRightKCellLeft
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content left))
	(not (exec (action guess) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 1)))))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," (+ ?y 1) "]"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (+ ?y 1))))
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 2)) (content water)))	;diag sopra dx
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 2)) (content water)))	;diag sotto dx

	(assert (cell_status (kx ?x) (ky (+ ?y 1)) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Effettua una GUESS su una cella posizionata a sinistra di una K-CELL con contenuto RIGHT
(defrule guessCellOnLeftKCellRIght
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content right))
	(not (exec (action guess) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 1)))))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	; GUESS sulla cella a SX della K-CELL con contenuto RIGHT
	(printout t "Step " ?s ":    GUESS cell [" ?x "," (- ?y 1) "]"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (- ?y 1))))
	(assert (k-cell (x (- ?x 1)) (y (- ?y 2)) (content water))) ; sopra-sx
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 2)) (content water))) ; sotto-sx

	(assert (cell_status (kx ?x) (ky (- ?y 1)) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; ################ GUESS DELLE CELLE DATO MIDDLE ##########################

; Se la K-CELL MIDDLE ha water a DX o SX e la cella sopra non e' stata GUESSED, allora GUESS su di essa
(defrule guessOnTopMiddleKCell
	(status (step ?s) (currently running))
	(moves (guesses ?ng &:(> ?ng 0)))
	(k-cell (x ?x) (y ?y) (content ?c &:(eq ?c middle)))
	(or 
		(k-cell (x ?x) (y ?y-left &:(eq ?y-left(- ?y 1))) (content water))
		(k-cell (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 1))) (content water))
	)
	(not (exec (action guess) (x ?x-up &:(eq ?x-up(- ?x 1))) (y ?y)))
	(not (exec (action fire) (x ?x-up &:(eq ?x-up(- ?x 1))) (y ?y)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" (- ?x 1) ", " ?y "] knowing [" ?x "," ?y "] middle"crlf)
	(assert (exec (step ?s) (action guess) (x (- ?x 1))(y ?y)))
	; ASSERISCO ACQUA ATTORNO
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water) ) )       ; dx
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water) ) )       ; sx
	(assert (k-cell (x (- ?x 2)) (y (+ ?y 1)) (content water) ) ) ; sopra-dx
	(assert (k-cell (x (- ?x 2)) (y (- ?y 1)) (content water) ) ) ; sopra-sx

	(assert (cell_status (kx (- ?x 1)) (ky  ?y ) (stat guessed)) ) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Se la K-CELL MIDDLE ha water a DX o SX e la cella sotto non e' stata GUESSED, allora GUESS su di essa
(defrule guessOnBotMiddleKCell
	(status (step ?s) (currently running))
	(moves (guesses ?ng &:(> ?ng 0)))
	(k-cell (x ?x) (y ?y) (content ?c &:(eq ?c middle)))
	(or 
		(k-cell (x ?x) (y ?y-left &:(eq ?y-left(- ?y 1))) (content water))
		(k-cell (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 1))) (content water))
	)
	(not (exec (action guess) (x ?x-bot &:(eq ?x-bot(+ ?x 1))) (y ?y)))
	(not (exec (action fire) (x ?x-bot &:(eq ?x-bot(+ ?x 1))) (y ?y)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" (+ ?x 1) ", " ?y "] knowing [" ?x "," ?y "] middle"crlf)
	(assert (exec (step ?s) (action guess) (x (+ ?x 1)) (y ?y)))
	; ASSERISCO ACQUA ATTORNO
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water) ) )       ; dx
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water) ) )       ; sx
	(assert (k-cell (x (+ ?x 2)) (y (+ ?y 1)) (content water) ) ) ; sotto-dx
	(assert (k-cell (x (+ ?x 2)) (y (- ?y 1)) (content water) ) ) ; sotto-sx

	(assert (cell_status (kx (+ ?x 1)) (ky ?y ) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Se la K-CELL MIDDLE ha water sopra o sotto e la cella a SX non e' stata GUESSED, allora GUESS su di essa
(defrule guessOnLeftMiddleKCell
	(status (step ?s) (currently running))
	(moves (guesses ?ng &:(> ?ng 0)))
	(k-cell (x ?x) (y ?y) (content ?c &:(eq ?c middle)))
	(or 
		(k-cell (x ?x-top &:(eq ?x-top (- ?x 1))) (y ?y) (content water))
		(k-cell (x ?x-bot &:(eq ?x-bot (+ ?x 1))) (y ?y) (content water))
	)
	(not (exec (action guess) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 1)))))
	(not (exec (action fire) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 1)))))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x ", " (- ?y 1) "] knowing [" ?x "," ?y "] middle"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (- ?y 1)) ))
	; ASSERISCO ACQUA ATTORNO
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water) ) )       ; sotto
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water) ) )       ; sopra
	(assert (k-cell (x (- ?x 1)) (y (- ?y 2)) (content water) ) ) ; sopra-sx
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 2)) (content water) ) ) ; sotto-sx

	(assert (cell_status (kx ?x) (ky (- ?y 1)) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

; Se la K-CELL MIDDLE ha water sopra o sotto e la cella a DX non e' stata GUESSED, allora GUESS su di essa
(defrule guessOnRightMiddleKCell
	(status (step ?s) (currently running))
	(moves (guesses ?ng &:(> ?ng 0)))
	(k-cell (x ?x) (y ?y) (content ?c &:(eq ?c middle)))
	(or 
		(k-cell (x ?x-top &:(eq ?x-top (- ?x 1))) (y ?y) (content water))
		(k-cell (x ?x-bot &:(eq ?x-bot (+ ?x 1))) (y ?y) (content water))
	)
	(not (exec (action guess) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 1)))))
	(not (exec (action fire) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 1)))))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell[" ?x ", " (+ ?y 1) "] knowing [" ?x "," ?y "]  middle"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (+ ?y 1))))
	; ASSERISCO ACQUA ATTORNO
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water) ) )       ; sotto
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water) ) )       ; sopra
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 2)) (content water) ) ) ; sopra-dx
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 2)) (content water) ) ) ; sotto-dx

	(assert (cell_status (kx ?x) (ky (+ ?y 1)) (stat guessed) )) ; tiene traccia che la cella è stata guessed
	(focus MAIN)
)

(defrule forcedGuessRowSingleUnknown (declare (salience 50))
  (status (step ?s) (currently running))
  
  (agent-k-per-row (row ?r) (num 1))
  
  ; Find exactly one unknown cell in row ?r
  ?cell <- (agent-cell (x ?r) (y ?y) (content unknown))
  
  ; No other unknown cells in the same row
  (not (agent-cell (x ?r) (y ?y2&:(<> ?y2 ?y)) (content unknown)))
  
  ; Check if not already guessed or fired there
  (not (cell_status (kx ?r) (ky ?y) (stat guessed)))
  (not (exec (action guess) (x ?r) (y ?y)))
  
  =>
  (printout t "Step " ?s ": FORCED GUESS on row " ?r " cell " ?y " (only unknown with 1 boat left)" crlf)
  (assert (cell_status (kx ?r) (ky ?y) (stat guessed)))
  (modify ?cell (content guessed))
  (focus MAIN)
)

(defrule forcedGuessColSingleUnknown (declare (salience 50))
  (status (step ?s) (currently running))
  
  (agent-k-per-col (col ?c) (num 1))
  
  ; Find exactly one unknown cell in col ?c
  ?cell <- (agent-cell (x ?x) (y ?c) (content unknown))
  
  ; No other unknown cells in the same column
  (not (agent-cell (x ?x2&:(<> ?x2 ?x)) (y ?c) (content unknown)))
  
  ; Check if not already guessed or fired there
  (not (cell_status (kx ?x) (ky ?c) (stat guessed)))
  (not (exec (action guess) (x ?x) (y ?c)))
  
  =>
  (printout t "Step " ?s ": FORCED GUESS on col " ?c " cell " ?x " (only unknown with 1 boat left)" crlf)
  (assert (cell_status (kx ?x) (ky ?c) (stat guessed)))
  (modify ?cell (content guessed))
  (focus MAIN)
)

(defrule fallbackGuessAnyCell (declare (salience -1000))
  (status (step ?s) (currently running))
  (moves (fires ?nf &:(= ?nf 0)))
  (not (exec (step ?s) (action guess) (x ?x) (y ?y))) ; only one guess per step
  (agent-k-per-row (row ?x) (num ?nr&:(> ?nr 0)))
  (agent-k-per-col (col ?y) (num ?nc&:(> ?nc 0)))
  ?cell <- (agent-cell (x ?x) (y ?y) (content unknown))
  =>
  (assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
  (modify ?cell (content guessed))  ; Mark cell as guessed
  (assert (cell_status (kx ?x) (ky ?y) (stat guessed)))
  (printout t "AGENT, step: " ?s ": fallback guessed cell [ " ?x " , " ?y " ]" crlf)
  (focus MAIN)
)

;(defrule emergencyGuessAnyValidCell
;  (declare (salience -2000))
;  (status (step ?s) (currently running))
;  (moves (guesses ?ng &:(> ?ng 0)))
;  (agent-cell (x ?x) (y ?y) (content unknown))
;  (test (and (>= ?x 0) (< ?x 10) (>= ?y 0) (< ?y 10)))
;  (not (exec (action guess) (x ?x) (y ?y)))
;=>
;  (printout t "!!! EMERGENCY: No better move found, GUESSING [" ?x ", " ?y "]" crlf)
;  (assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
;  (assert (cell_status (kx ?x) (ky ?y) (stat guessed)))
;  (focus MAIN)
;)
