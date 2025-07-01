; ################################ FIRE VALUATION MODULE #######################################

(defmodule FIRE_VAL (import MAIN ?ALL) (import ENV ?ALL) (import AGENT ?ALL) (export ?ALL))

; If there are still cruisers then recalls the FIRE module
(defrule fireIfCruiser
	(status (step ?s)(currently running))
	(cruiser (to_find ?to_find_c &:(> ?to_find_c 0)))
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(focus FIRE_MOD)
)

; If there are still battleships, then recalls the FIRE module
(defrule fireIfBattleship
	(status (step ?s)(currently running))
	(battleship (to_find ?to_find_b &:(> ?to_find_b 0)))
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(focus FIRE_MOD)
)

; Fallback rule: fire on the cell with the highest k-row and k-col
(defrule fireOnCellBestRowAndCol 
	(status (step ?s) (currently running))
	(k-per-row (row ?x) (num ?numR &:(> ?numR 0)))
	(not (k-per-row (row ?x2) (num ?num-r2 &:(> ?num-r2 ?numR))))
	(k-per-col (col ?y) (num ?numC &:(> ?numC 0)))
	(not (k-per-col (col ?y2) (num ?numC2 &:(> ?numC2 ?numC))))
	(moves (fires ?nf &:(> ?nf 0)))
	(not (k-cell (x ?x) (y ?y)))
	(not (exec (action fire) (x ?x) (y ?y)))
	(not (exec (action guess) (x ?x) (y ?y)))
=>
	(printout t crlf)
	(printout t "Step " ?s ":    FIRE ON [" ?x ", " ?y "] based on best K-ROW and K-COL" crlf)
	(assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
	(focus MAIN)
)