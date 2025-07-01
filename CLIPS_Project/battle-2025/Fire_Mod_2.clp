
;  #######################  MODULO ESECUSIONE FIRE ##############################

(defmodule FIRE_MOD (import MAIN ?ALL) (import ENV ?ALL) (import AGENT ?ALL) (import FIRE_VAL ?ALL) (export ?ALL))


; Viene eseguita una FIRE 2 celle sotto alla K-CELL con contenuto TOP
(defrule fire2CellUnderKCellTop 
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content top))
	(not (exec (action fire) (x ?x-2under &:(eq ?x-2under(+ ?x 2))) (y ?y) )) ; Non e' eseguita la fire 
	(not (exec (action guess) (x ?x-2under &:(eq ?x-2under(+ ?x 2))) (y ?y) )) ; Non e' stata guessed
	(not (k-cell (x ?x-2under &:(eq ?x-2under(+ ?x 2))) (y ?y) (content water))) ; Controlla che cella non abbia contenuto WATER 
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(assert (exec (step ?s) (action fire) (x (+ ?x 2)) (y ?y)))
	(printout t crlf)
	(printout t "Step " ?s ":    FIRE cell [" (+ ?x 2) "," ?y "] knowing [" ?x "," ?y "] top" crlf)
	(assert (cell_status  (stat fired) (kx (+ ?x 2)) (ky ?y) )) ; Tiene traccia che la cella e' stata fired
	(focus MAIN)
)

; Viene eseguita una FIRE 2 celle sopra alla K-CELL con contenuto BOTTOM
(defrule fire2CellOverKCellBot 
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content bot))
	(not (exec (action fire) (x ?x-2up &:(eq ?x-2up(- ?x 2))) (y ?y) )) ; Non e' eseguita la fire  
	(not (exec (action guess) (x ?x-2up &:(eq ?x-2up(- ?x 2))) (y ?y) ))  ; Non e' stata guessed
	(not (k-cell (x ?x-2under &:(eq ?x-2under(- ?x 2))) (y ?y) (content water))) ; Controlla che cella non abbia contenuto WATER
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(assert (exec (step ?s) (action fire) (x (- ?x 2)) (y ?y) ))
	(printout t crlf)
	(printout t "Step " ?s ":    FIRE cell [" (- ?x 2) "," ?y "] knowing [" ?x "," ?y "] bot" crlf)
	(assert (cell_status  (stat fired) (kx (- ?x 2)) (ky ?y) )) ; Tiene traccia che la cella e' stata fired
	(focus MAIN)
)

; Viene eseguita una FIRE 2 celle a destra della K-CELL con contenuto LEFT
(defrule fire2CellDxKCellLeft
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content left))
	(not (exec (action fire) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 2))) )) ; Non e' eseguita la fire
	(not (exec (action guess) (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 2))) )) ; Non e' stata guessed
	(not (k-cell (x ?x) (y ?y-right &:(eq ?y-right(+ ?y 2))) (content water))) ; Controlla che cella non abbia contenuto WATER
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(assert (exec (step ?s) (action fire) (x ?x)(y (+ ?y 2)) ))       
	(printout t crlf)
	(printout t "Step " ?s ":    FIRE cell [" ?x "," (+ ?y 2) "] knowing [" ?x "," ?y "] left" crlf)
	(assert (cell_status  (stat fired) (kx ?x)(ky (+ ?y 2)) )) ; Tiene traccia che la cella e' stata fired
	(focus MAIN)
)

; FIRE e GUESS su 2 celle a sx alla K-CELL con content=RIGHT
(defrule fire2CellSxKCellRight
	(status (step ?s) (currently running))
	(k-cell (x ?x) (y ?y) (content right))
	(not (exec (action fire) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 2))) )) ; Non e' eseguita la fire 
	(not (exec (action guess) (x ?x) (y ?y-left &:(eq ?y-left(- ?y 2))) )) ; Non e' stata guessed
	(not (k-cell (x ?x) (y ?y-left &:(eq ?y-left(- ?y 2))) (content water))) ; Controlla che cella non abbia contenuto WATER
	(moves (fires ?nf &:(> ?nf 0)))
=>
	(assert (exec (step ?s) (action fire) (x ?x)(y (- ?y 2)) ))       
	(printout t crlf)
	(printout t "Step " ?s ":    FIRE cell [" ?x "," (- ?y 2) "] knowing [" ?x "," ?y "] right" crlf)
	(assert (cell_status  (stat fired) (kx ?x)(ky (- ?y 2)) )) ; Tiene traccia che la cella e' stata fired
	(focus MAIN)
)

; fallback rules 
(defrule fireOnMostProbableCell
  (declare (salience -1000))
  (status (step ?s) (currently running))
  (moves (fires ?nf &:(> ?nf 0)))

  ; Best row and column based on agent beliefs
  (agent-k-per-row (row ?x) (num ?nr&:(> ?nr 0)))
  (not (agent-k-per-row (row ?x2) (num ?nr2&:(> ?nr2 ?nr))))

  (agent-k-per-col (col ?y) (num ?nc&:(> ?nc 0)))
  (not (agent-k-per-col (col ?y2) (num ?nc2&:(> ?nc2 ?nc))))

  ; Make sure the cell is valid and unknown
  (agent-cell (x ?x) (y ?y) (content unknown))
  (not (exec (action fire) (x ?x) (y ?y)))
  (not (exec (action guess) (x ?x) (y ?y)))
=>
  (printout t crlf)
  (printout t "Step " ?s ": fallback FIRE on most probable cell [" ?x ", " ?y "]" crlf)
  (assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
  (focus MAIN)
)


;(defrule fireOnBestRow
;	(status (step ?s) (currently running))
;	(k-per-row (row ?x) (num ?numR &:(> ?numR 0)))
;	(not (k-per-row (row ?x2) (num ?num-r2 &:(> ?num-r2 ?numR))))
;	(moves (fires ?nf &:(> ?nf 0)))
;	?index <- (indexFire (i ?i))
;	(not (k-cell (x ?x) (y ?i)))
;	(not (exec (action fire) (x ?x) (y ?i)))
;	(not (exec (action guess) (x ?x) (y ?i)))
;=>
;	(printout t crlf)
;	(printout t "Step " ?s ":    FIRE ON [" ?x ", " ?i "] based on best K-ROW" crlf)
;	(assert (exec (step ?s) (action fire) (x ?x) (y ?i)))
;	(modify ?index (i (+ ?i 2))) ; Aumento indice di due: prossima fire stessa riga, ma due celle a dx
;	(focus MAIN)
;)

(defrule emergencyFireAnyValidCell
  (declare (salience -2000))
  (status (step ?s) (currently running))
  (moves (fires ?nf &:(> ?nf 0)))
  (agent-cell (x ?x) (y ?y) (content unknown))
  (test (and (>= ?x 0) (< ?x 10) (>= ?y 0) (< ?y 10)))
  (not (exec (action fire) (x ?x) (y ?y)))
=>
  (printout t "!!! EMERGENCY: No better move found, FIRING on [" ?x ", " ?y "]" crlf)
  (assert (exec (step ?s) (action fire) (x ?x) (y ?y)))
  (focus MAIN)
)