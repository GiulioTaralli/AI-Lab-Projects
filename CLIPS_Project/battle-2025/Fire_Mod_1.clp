; ################################ FIRE EXECUTION MODULE #######################################

(defmodule FIRE_MOD (import MAIN ?ALL) (import ENV ?ALL) (import AGENT ?ALL) (import FIRE_VAL ?ALL) (export ?ALL))

; Fire at 2 cells under the K-cell with top content
(defrule fire2CellUnderKCellTop 
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content top))
	(not (exec (action fire) (x ?x-2under &:(eq ?x-2under(+ ?x 2))) (y ?y))) ; It is not fired 
	(not (exec (action guess) (x ?x-2under &:(eq ?x-2under(+ ?x 2))) (y ?y))) ; It is not guessed
	(not (k-cell (x ?x-2under &:(eq ?x-2under(+ ?x 2))) (y ?y) (content water))) ; Check the cell is not water
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(assert (exec (step ?s) (action fire) (x (+ ?x 2)) (y ?y)))
	(printout t crlf)
	(printout t "Step " ?s ":    FIRE cell [" (+ ?x 2) "," ?y "] knowing [" ?x "," ?y "] top" crlf)
	(assert (cell_status  (stat fired) (kx (+ ?x 2)) (ky ?y))) ; Keeps track that the cell has been fired
	(focus MAIN)
)

; Fire at 2 cells up the K-cell with bottom content
(defrule fire2CellOverKCellBot 
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content bot))
	(not (exec (action fire) (x ?x-2up &:(eq ?x-2up(- ?x 2))) (y ?y)))   
	(not (exec (action guess) (x ?x-2up &:(eq ?x-2up(- ?x 2))) (y ?y)))  
	(not (k-cell (x ?x-2under &:(eq ?x-2under(- ?x 2))) (y ?y) (content water))) 
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(assert (exec (step ?s) (action fire) (x (- ?x 2)) (y ?y)))
	(printout t crlf)
	(printout t "Step " ?s ": FIRE cell [" (- ?x 2) "," ?y "] knowing [" ?x "," ?y "] bot" crlf)
	(assert (cell_status  (stat fired) (kx (- ?x 2)) (ky ?y)))
	(focus MAIN)
)

; Fire at 2 cells right the K-cell with left content
(defrule fire2CellDxKCellLeft
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content left))
	(not (exec (action fire) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 2))))) 
	(not (exec (action guess) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 2))))) 
	(not (k-cell (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 2))) (content water))) 
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(assert (exec (step ?s) (action fire) (x ?x)(y (+ ?y 2))))       
	(printout t crlf)
	(printout t "Step " ?s ": FIRE cell [" ?x "," (+ ?y 2) "] knowing [" ?x "," ?y "] left" crlf)
	(assert (cell_status  (stat fired) (kx ?x)(ky (+ ?y 2)))) 
	(focus MAIN)
)

; Fire at 2 cells left the K-cell with right content
(defrule fire2CellSxKCellRight
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content right))
	(not (exec (action fire) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 2)))))
	(not (exec (action guess) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 2)))))
	(not (k-cell (x ?x) (y ?y-left &:(eq ?y-left(- ?y 2))) (content water)))
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(assert (exec (step ?s) (action fire) (x ?x)(y (- ?y 2))))       
	(printout t crlf)
	(printout t "Step " ?s ": FIRE cell [" ?x "," (- ?y 2) "] knowing [" ?x "," ?y "] right" crlf)
	(assert (cell_status  (stat fired) (kx ?x)(ky (- ?y 2))))
	(focus MAIN)
)