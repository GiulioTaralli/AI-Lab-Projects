; ################################## MODULO GUESS ############################################

(defmodule GUESS_MOD (import MAIN ?ALL) (import ENV ?ALL) (import AGENT ?ALL) (export ?ALL))

; ############## GUESS ON THE KNOWING CELLS ###############

; Make a Guess on a K-Cell with top content
(defrule guessTopKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content top))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," ?y "] top"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	;guess K-CELL TOP
	; Assert water around the cell
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) 	; up
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water)))	; left
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water)))	; right
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	; up diag left
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	; up diag right
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	; down diag left
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	; down diag right
	; Keeps track that the cell was guessed
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed)))
	(focus MAIN)
)

; Make a Guess on a cell positioned under a K-Cell with top content
(defrule guessCellUnderTopKCell
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content top))
	(not (exec (action guess) (x ?x-under &:(eq ?x-under(+ ?x 1))) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" (+ ?x 1) "," ?y "]"crlf)
	(assert (exec (step ?s) (action guess) (x (+ ?x 1)) (y ?y)))
	(assert (k-cell (x (+ ?x 2)) (y (- ?y 1)) (content water)))	; down diag left
	(assert (k-cell (x (+ ?x 2)) (y (+ ?y 1)) (content water)))	;down diag right
	; Keeps track that the cell was guessed
	(assert (cell_status (kx (+ ?x 1)) (ky ?y) (stat guessed)))
	(focus MAIN)
)

; Make a Guess on a K-Cell with Bottom content
(defrule guessBottomKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content bot))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," ?y "] bot"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))	;guess K-CELL BOT
	; Assert water around the cell
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) ; down
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water))) ; left
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water))) ; right
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	; up diag left
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	; up diag right
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	; down diag left
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	; down diag right
	; Keeps track that the cell was guessed
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; Make a Guess on a cell positioned on a K-Cell with Bottom content
(defrule guessCellOnTopBottomKCell 
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content bot))
	(not (exec (action guess) (x ?x-top &:(eq ?x-top(- ?x 1))) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" (- ?x 1) "," ?y "]"crlf)
	(assert (exec (step ?s) (action guess) (x (- ?x 1)) (y ?y)))
	(assert (k-cell (x (- ?x 2)) (y (- ?y 1)) (content water)))	; down diag left
	(assert (k-cell (x (- ?x 2)) (y (+ ?y 1)) (content water)))	; down diag right
	; Keeps track that the cell was guessed
	(assert (cell_status (kx (- ?x 1)) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; Make a Guess on a K-Cell with Right Content
(defrule guessRightKCell (declare (salience 100))
	(status (step ?s)(currently running))
	(k-cell (x ?x) (y ?y) (content right))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=> 
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," ?y "] right"crlf)
	(assert (exec(step ?s) (action guess) (x ?x) (y ?y))) ; current cell
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water))) ; right
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) ; down
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) ; up
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water))) ; up-right
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water))) ; up-left
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water))) ; down-right
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water))) ; down-left
	; Keeps track that the cell was guessed
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed)))
	(focus MAIN)
)

; Make a Guess on a cell positioned to the left of a K-Cell with right content
(defrule guessCellOnLeftKCellRIght
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content right))
	(not (exec (action guess) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 1)))))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," (- ?y 1) "]"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (- ?y 1))))
	(assert (k-cell (x (- ?x 1)) (y (- ?y 2)) (content water))) ; up-left
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 2)) (content water))) ; down-left
	; Keeps track that the cell was guessed
	(assert (cell_status (kx ?x) (ky (- ?y 1)) (stat guessed))) 
	(focus MAIN)
)

; Make a Guess on a K-Cell with left content
(defrule guessLeftKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content left))
	(not (exec (action guess) (x ?x) (y ?y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," ?y "] left"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
	; Assert water around the K-cell
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) ; down
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) ; up
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water))) ; left
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	; up diag left
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	; up diag right
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	; down diag left
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	; down diag right
	; Keeps track that the cell was guessed
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed)))
	(focus MAIN)
)

; Make a Guess on a cell positioned to the right of a K-Cell with left content
(defrule guessCellOnRightKCellLeft
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content left))
	(not (exec (action guess) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 1)))))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," (+ ?y 1) "]"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (+ ?y 1))))
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 2)) (content water)))	; up diag right
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 2)) (content water)))	; down diag right
	; Keeps track that the cell was guessed
	(assert (cell_status (kx ?x) (ky (+ ?y 1)) (stat guessed))) 
	(focus MAIN)
)

; Make a Guess on a K-Cell with sub (submarine) content
(defrule guessSubKCell (declare (salience 100))
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content sub))
	(not (exec (action guess) (x ?x) (y ?y)))
	?stf <- (submarine (to_find ?to_find_s &:(> ?to_find_s 0)))
	(moves (guesses ?ng &:(> ?ng 0)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," ?y "] sub"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y ?y)))
    (modify ?stf (to_find (- ?to_find_s 1))) ; Decrease the number of submarines to find
	(printout t crlf)
	(printout t "SUBMARINE FOUND!!")
	(printout t crlf)
	; Assert water around the cell
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) ; down
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) ; up
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water))) ; left
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water))) ; right
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water)))	; up diag left
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water)))	; up diag right
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water)))	; down diag left
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water)))	; down diag right
	; Keeps track that the cell was guessed
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; Make a Guess on a K-Cell with middle content
(defrule guessMiddleKCell (declare (salience 100))
	(status (step ?s)(currently running))
	(k-cell (x ?x) (y ?y) (content middle))
	(not (exec (action guess) (x ?x) (y ?Y)))
	(moves (guesses ?ng &:(> ?ng 0)))
=> 
	(printout t crlf)
	(printout t "Step " ?s ":    GUESS cell [" ?x "," ?y "] middle"crlf)
	(assert (exec(step ?s) (action guess) (x ?x) (y ?y))) ; this cell
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 1)) (content water))) ; up-right
	(assert (k-cell (x (- ?x 1)) (y (- ?y 1)) (content water))) ; up-left
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 1)) (content water))) ; down-right
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 1)) (content water))) ; down-left
	; Keeps track that the cell was guessed
	(assert (cell_status (kx ?x) (ky ?y) (stat guessed))) 
	(focus MAIN)
)

; ################ GUESS ON THE NEAR MIDDLE CELLS ##########################

; If the K-Cell middle has a water to the right or left and the cell above it was not guessed, 
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
	(printout t "Step " ?s ":    GUESS cell [" (- ?x 1) ", " ?y "] knowing [" ?x "," ?y "] middle"crlf)
	(assert (exec (step ?s) (action guess) (x (- ?x 1))(y ?y)))
	; Assert water around the cell
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water))) ; right
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water))) ; left
	(assert (k-cell (x (- ?x 2)) (y (+ ?y 1)) (content water))) ; up-right
	(assert (k-cell (x (- ?x 2)) (y (- ?y 1)) (content water))) ; up-left
	(focus MAIN)
)

; If the K-Cell middle has a water to the right or left and the cell below was not guessed, 
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
	(printout t "Step " ?s ":    GUESS cell [" (+ ?x 1) ", " ?y "] knowing [" ?x "," ?y "] middle"crlf)
	(assert (exec (step ?s) (action guess) (x (+ ?x 1)) (y ?y)))
	; Assert water around the cell
	(assert (k-cell (x ?x) (y (+ ?y 1)) (content water))) ; right
	(assert (k-cell (x ?x) (y (- ?y 1)) (content water))) ; left
	(assert (k-cell (x (+ ?x 2)) (y (+ ?y 1)) (content water))) ; up-right
	(assert (k-cell (x (+ ?x 2)) (y (- ?y 1)) (content water))) ; up-left
	(focus MAIN)
)

; If the K-Cell middle has a water above or below and the cell to the left was not guessed, 
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
	(printout t "Step " ?s ":    GUESS cell [" ?x ", " (- ?y 1) "] knowing [" ?x "," ?y "] middle"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (- ?y 1)) ))
	; Assert water around the cell
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) ; down
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) ; up
	(assert (k-cell (x (- ?x 1)) (y (- ?y 2)) (content water))) ; up-left
	(assert (k-cell (x (+ ?x 1)) (y (- ?y 2)) (content water))) ; down-left
	(focus MAIN)
)

; If the K-Cell Middle has a water above or below and the cell on the right was not guessed, 
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
	(printout t "Step " ?s ":    GUESS cell[" ?x ", " (+ ?y 1) "] knowing [" ?x "," ?y "]  middle"crlf)
	(assert (exec (step ?s) (action guess) (x ?x) (y (+ ?y 1))))
	; Assert water around the cell
	(assert (k-cell (x (+ ?x 1)) (y ?y) (content water))) ; down
	(assert (k-cell (x (- ?x 1)) (y ?y) (content water))) ; up
	(assert (k-cell (x (- ?x 1)) (y (+ ?y 2)) (content water))) ; up-right
	(assert (k-cell (x (+ ?x 1)) (y (+ ?y 2)) (content water))) ; down-right
	(focus MAIN)
)