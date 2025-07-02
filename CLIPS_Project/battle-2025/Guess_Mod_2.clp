; ################################## GUESS MODULE ##################################

(defmodule GUESS_MOD (import MAIN ?ALL) (import ENV ?ALL) (import AGENT ?ALL) (export ?ALL))

; ######################## GUESS ON THE KNOWN CELL ########################

; Make a guess on a K-cell with top content
(defrule guessTopKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content top))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" ?x "," ?y "] top"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	
	; Assert water near the K-cell
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) 	
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed)))
	(focus MAIN)
)

; Make a guess on a K-cell with bottom content
(defrule guessBottomKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content bot))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" ?x "," ?y "] bot"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	
	; Assert water near the K-cell
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) 	
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed)))
	(focus MAIN)
)

; Make a guess on a K-cell with left content
(defrule guessLeftKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content left))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" ?x "," ?y "] left"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	
	; Assert water near the K-cell
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) 	
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) 	
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; Make a guess on a K-cell with sub (submarine) content
(defrule guessKCellSub (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content sub))
	(not (exec (action guess) (x ?x) (y ?y)))
	?stf <- (submarine (to_find ?to_find_s &:(> ?to_find_s 0)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" ?x "," ?y "] sub"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	
    (modify ?stf (to_find (- ?to_find_s 1))) 
	(printout t crlf)
	(printout t "SUBMARINE FOUND!!")
	(printout t crlf)
	; Assert water all around the k-cell
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) 	
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) 	
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; Make a guess on a K-cell with right content
(defrule guessRightKCell (declare (salience 100))
	(status (step ?s)(currently running))
	(k-cell (x ?x) (y ?y) (content right))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=> 
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" ?x "," ?y "] right"crlf)
	(assert (exec(step ?s) (action guess) (x ?x) (y ?y)))
	; Assert water near the K-cell		
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))       
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water)))       
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water)))       
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water))) 
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water))) 
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water))) 
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water))) 
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; Make a guess on a K-cell with middle content
(defrule guessMiddleKCell (declare (salience 100))
	(status (step ?s)(currently running))
	(k-cell (x ?x) (y ?y) (content middle))
	(not (exec (action guess) (x ?x) (y ?Y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=> 
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" ?x "," ?y "] middle"crlf)
	(assert (exec(step ?s) (action guess) (x ?x) (y ?y)))
	; Assert water near the K-cell		
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water))) 
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water))) 
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water))) 
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water))) 
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; ################### GUESS NEXT TO KNWON CELLS ###################

; Make a guess on a cell placed under a K-cell with top content
(defrule guessCellUnderTopKCell 
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content top))
	(not (exec (action guess) (x ?x-under &:(eq ?x-under(+ ?x 1))) (y ?y))) 
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" (+ ?x 1) "," ?y "]"crlf)
	(assert (exec (step ?s) (action guess) (x (+ ?x 1)) (y ?y)))
	; Assert water
	(assert (k-cell (x (+ ?x 2)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (+ ?x 2)) (y (+ ?y 1)) (content water)))	
	(assert (cell_status (kx (+ ?x 1)) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; Make a guess on a cell placed above a K-cell with bottom content
(defrule guessCellOnTopBottomKCell
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content bot))
	(not (exec (action guess) (x ?x-top &:(eq ?x-top(- ?x 1))) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" (- ?x 1) "," ?y "]"crlf)
	(assert (exec (step ?s) (action guess) (x (- ?x 1)) (y ?y)))
	; Assert water
	(assert (k-cell (x (- ?x 2)) (y (- ?y 1)) (content water)))	
	(assert (k-cell (x (- ?x 2)) (y (+ ?y 1)) (content water)))	
	(assert (cell_status (kx (- ?x 1)) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; Make a guess on a cell placed right a K-cell with left content
(defrule guessCellOnRightKCellLeft
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content left))
	(not (exec (action guess) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 1)))))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" ?x "," (+ ?y 1) "]"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (+ ?y 1))))
	; Assert water
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 2)) (content water)))	
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 2)) (content water)))	
	(assert (cell_status (kx ?x) (ky (+ ?y 1)) (stat guessed))) 
	(focus MAIN)
)

; Make a guess on a cell placed left a K-cell with right content
(defrule guessCellOnLeftKCellRIght
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content right))
	(not (exec (action guess) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 1)))))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": GUESS cell [" ?x "," (- ?y 1) "]"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (- ?y 1))))
	; Assert water
	(assert (k-cell (x (- ?x 1)) (y (- ?y 2)) (content water))) 
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 2)) (content water))) 
	(assert (cell_status (kx ?x) (ky (- ?y 1)) (stat guessed))) 
	(focus MAIN)
)

; ########################## MANAGEMENT GUESS ON MIDDLE CELLS ##########################

; If the K-cell middle has water on the right or left and the cell above has not been guessed, 
; then guess on it
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
	(printout t "Step " ?s ": GUESS cell [" (- ?x 1) ", " ?y "] knowing [" ?x "," ?y "] middle"crlf)
	(assert (exec (step ?s) (action guess) (x (- ?x 1))(y ?y)))
	; Assert water
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))       
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))       
	(assert (k-cell (x (- ?x 2)) (y (+ ?y 1)) (content water))) 
	(assert (k-cell (x (- ?x 2)) (y (- ?y 1)) (content water))) 
	(assert (cell_status (kx (- ?x 1)) (ky  ?y) (stat guessed))) 
	(focus MAIN)
)

; If the K-cell middle has water on the right or left and the cell down has not been guessed, 
; then guess on it
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
	(printout t "Step " ?s ": GUESS cell [" (+ ?x 1) ", " ?y "] knowing [" ?x "," ?y "] middle" crlf)
	(assert (exec (step ?s) (action guess) (x (+ ?x 1)) (y ?y)))
	; Assert water around
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))       
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))       
	(assert (k-cell (x (+ ?x 2)) (y (+ ?y 1)) (content water))) 
	(assert (k-cell (x (+ ?x 2)) (y (- ?y 1)) (content water))) 
	(assert (cell_status (kx (+ ?x 1)) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; If the K-cell middle has water on the up or down and the cell on left has not been guessed, 
; then guess on it
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
	(printout t "Step " ?s ": GUESS cell [" ?x ", " (- ?y 1) "] knowing [" ?x "," ?y "] middle"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (- ?y 1))))
	; Assert water around
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water)))       
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water)))       
	(assert (k-cell (x (- ?x 1)) (y (- ?y 2)) (content water))) 
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 2)) (content water))) 
	(assert (cell_status (kx ?x) (ky (- ?y 1)) (stat guessed))) 
	(focus MAIN)
)

; If the K-cell middle has water on the up or down and the cell on right has not been guessed, 
; then guess on it
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
	(printout t "Step " ?s ": GUESS cell[" ?x ", " (+ ?y 1) "] knowing [" ?x "," ?y "]  middle"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (+ ?y 1))))
	; Assert water around
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water)))       
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water)))       
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 2)) (content water))) 
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 2)) (content water))) 
	(assert (cell_status (kx ?x) (ky (+ ?y 1)) (stat guessed))) 
	(focus MAIN)
)

; If there is only one ship left in the row, and there is only one unknown cell, then guess on that one
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

; If there is only one ship left in the column, and there is only one unknown cell, then guess on that one
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

; Fallback guess
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
  (printout t "Step: " ?s ": fallback guessed cell [ " ?x " , " ?y " ]" crlf)
  (focus MAIN)
)