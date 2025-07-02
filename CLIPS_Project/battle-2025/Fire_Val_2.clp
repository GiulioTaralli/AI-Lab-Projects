; ############################  FIRE VALUATION MODULE ############################

(defmodule FIRE_VAL (import MAIN ?ALL) (import ENV ?ALL) (import AGENT ?ALL) (export ?ALL))

; If there are still cruisers then call the fire module
(defrule fireIfCruiser
	(status (step ?s)(currently running))
	(cruiser (to_find ?to_find_c &:(> ?to_find_c 0)))
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(focus FIRE_MOD)
)

; If there are still battleships then call the fire module
(defrule fireIfBattleship
	(status (step ?s)(currently running))
	(battleship (to_find ?to_find_b &:(> ?to_find_b 0)))
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(focus FIRE_MOD)
)

; If there is no more 3-cells or 4-cells ships, then fire on the cell with the highest k-row and k-col
(defrule fireOnCellBestRowAndCol 
	(status (step ?s) (currently running))
	(k-per-row (row ?x) (num ?numR &:(> ?numR 0)))
	(not (k-per-row (row ?x2) (num ?num-r2 &:(> ?num-r2 ?numR))))
	(k-per-col (col ?y) (num ?numC &:(> ?numC 0)))
	(not (k-per-col (col ?y2) (num ?numC2 &:(> ?numC2 ?numC)))  )
	(moves (fires ?nf &:(> ?nf 0)))
	(not (k-cell (x ?x) (y ?y)))
	(not (exec (action fire) (x ?x) (y ?y)))
	(not (exec (action guess) (x ?x) (y ?y)))
=>
	(printout t crlf)
	(printout t "Step " ?s ": FIRE ON [" ?x ", " ?y "] based on best K-ROW and K-COL" crlf)
	(assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
	(focus MAIN)
)

; Fallback rule: fire on the cell with the highest k-row and k-col
(defrule fireOnMostProbableCell
  (declare (salience -1000))
  (status (step ?s) (currently running))
  (moves (fires ?nf &:(> ?nf 0)))

  ; Find the best row
  (agent-k-per-row (row ?x) (num ?nr&:(> ?nr 0)))
  (not (agent-k-per-row (row ?x2) (num ?nr2&:(> ?nr2 ?nr))))

  ; Find the best column
  (agent-k-per-col (col ?y) (num ?nc&:(> ?nc 0)))
  (not (agent-k-per-col (col ?y2) (num ?nc2&:(> ?nc2 ?nc))))

  ; Make sure this cell hasn't been touched and is unknown
  (agent-cell (x ?x) (y ?y) (content unknown))
  (not (exec (action fire) (x ?x) (y ?y)))
  (not (exec (action guess) (x ?x) (y ?y)))
=>
  (printout t crlf)
  (printout t "Step " ?s ": fallback FIRE on most probable cell [" ?x ", " ?y "]" crlf)
  (assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
  (focus MAIN)
)

; If in a row there is only one ship to find, and there are only two unknown cells, 
; then fire on one of the two
(defrule fireOnRowWithTwoUnknownCells (declare (salience 200))
  (status (step ?s) (currently running))
  (moves (fires ?nf &:(> ?nf 0)))

  ; Find a row ?r with exactly 2 unknown cells, match two distinct unknown cells in the same row ?r
  ?cell1 <- (agent-cell (x ?r) (y ?y1&:(>= ?y1 0) &:(<= ?y1 9)) (content unknown))
  ?cell2 <- (agent-cell (x ?r) (y ?y2&:(>= ?y2 0) &:(<= ?y2 9)) (content unknown &:(<> ?y2 ?y1)))
  
  ; Ensure no third unknown cell in that row
  (not (agent-cell (x ?r) (y ?y3&:(and (<> ?y3 ?y1) (<> ?y3 ?y2))) (content unknown)))

  ; Make sure cells not guessed/fired
  (not (exec (action fire) (x ?r) (y ?y1)))
  (not (exec (action fire) (x ?r) (y ?y2)))
  (not (exec (action guess) (x ?r) (y ?y1)))
  (not (exec (action guess) (x ?r) (y ?y2)))

=>
  (printout t "Step " ?s ": FIRE on row " ?r " cell " ?y1 " (row with exactly 2 unknown cells)" crlf)
  (assert (exec (step ?s) (action fire) (x ?r) (y ?y1)))
  (focus MAIN)
)

; If in a column there is only one ship to find, and there are only two unknown cells, 
; then fire on one of the two
(defrule fireOnColWithTwoUnknownCells (declare (salience 200)) 
  (status (step ?s) (currently running))
  (moves (fires ?nf &:(> ?nf 0)))

  ; Match two distinct unknown cells in the same column ?c
  ?cell1 <- (agent-cell (x ?x1&:(>= ?x1 0) &:(<= ?x1 9)) (y ?c) (content unknown))
  ?cell2 <- (agent-cell (x ?x2&:(>= ?x2 0) &:(<= ?x2 9)) (y ?c) (content unknown &:(<> ?x2 ?x1)))

  ; Ensure no third unknown cell in that column
  (not (agent-cell (x ?x3&:(and (<> ?x3 ?x1) (<> ?x3 ?x2))) (y ?c) (content unknown)))

  ; Make sure cells not guessed/fired
  (not (exec (action fire) (x ?x1) (y ?c)))
  (not (exec (action fire) (x ?x2) (y ?c)))
  (not (exec (action guess) (x ?x1) (y ?c)))
  (not (exec (action guess) (x ?x2) (y ?c)))
=>
  (printout t "Step " ?s ": FIRE on col " ?c " cell " ?x1 " (col with exactly 2 unknown cells)" crlf)
  (assert (exec (step ?s) (action fire) (x ?x1) (y ?c)))
  (focus MAIN)
)

; Fallaback fire
(defrule emergencyFireAnyValidCell
  (declare (salience -2000))
  (status (step ?s) (currently running))
  (moves (fires ?nf &:(> ?nf 0)))
  (agent-cell (x ?x) (y ?y) (content unknown))
  (test (and (>= ?x 0) (< ?x 10) (>= ?y 0) (< ?y 10)))
  (not (exec (action fire) (x ?x) (y ?y)))
=>
  (printout t "Step " ?s ": fallback, no better move found, FIRE on [" ?x ", " ?y "]" crlf)
  (assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
  (focus MAIN)
)