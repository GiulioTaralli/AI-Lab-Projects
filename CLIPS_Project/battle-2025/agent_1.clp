; ####################################### AGENT 1 #######################################

(defmodule AGENT (import MAIN ?ALL) (import ENV ?ALL) (export ?ALL))

; ######################## AGENT FACTS ########################

; Number of ships still to be discovered by type
(deftemplate submarine	
	(slot to_find)
)

(deftemplate destroyer	
	(slot to_find)
)

(deftemplate cruiser	
	(slot to_find)
)

(deftemplate battleship
	(slot to_find)
)

(deffacts boat_to_find
	(submarine (to_find 4))
	(destroyer (to_find 3))
	(cruiser (to_find 2))
	(battleship (to_find 1))
)

; Fire index
(deftemplate indexFire
	(slot i)
)

(deffacts initIndex
	(indexFire (i 0))
)

; Template for checks on the cell
(deftemplate cell_status
	(slot kx)
	(slot ky)
	(slot stat (allowed-values none guessed fired))
)

; Template for controls on lines/columns
(deftemplate k-row-water
	(slot row)
)

(deftemplate k-col-water
	(slot col)
)

; Template to keep track of the cruisers found
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

; Template to keep track of the destroyers
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

; Template to keep track of the battleships found
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

; ######################## INITIALIZATION ########################

; Print in case there are no known cells at the beginning
(defrule beginning-no-knowledge (declare (salience 501))
	(not (k-cell (x ?x) (y ?y) (content ?t)))
=>
	(printout t "No known cells at the beginning" crlf)
)

; Assert water in the lines that have the K-Per-Row value of 0
(defrule initializeWaterKRow (declare (salience 500))
	(status (step ?s)(currently running))
	(k-per-row (row ?r) (num 0))
	(not (k-row-water (row ?r)))
=>
	(printout t "WATER on ROW " ?r crlf)
	(assert (k-cell (x ?r) (y 0) (content water)))
	(assert (k-cell (x ?r) (y 1) (content water)))
	(assert (k-cell (x ?r) (y 2) (content water)))
	(assert (k-cell (x ?r) (y 3) (content water)))
	(assert (k-cell (x ?r) (y 4) (content water)))
	(assert (k-cell (x ?r) (y 5) (content water)))
	(assert (k-cell (x ?r) (y 6) (content water)))
	(assert (k-cell (x ?r) (y 7) (content water)))
	(assert (k-cell (x ?r) (y 8) (content water)))
	(assert (k-cell (x ?r) (y 9) (content water)))
	(assert (k-row-water (row ?r)))
	(printout t crlf)
)

; Assert water in the columns that have the k-per-color value equal to 0
(defrule initializeWaterKCol (declare (salience 500))
	(status (step ?s)(currently running))
	(k-per-col (col ?c) (num 0))
	(not (k-col-water (col ?c)))
=>
	(printout t "WATER on COLUMN " ?c crlf)
	(assert (k-cell (x 0) (y ?c) (content water)))
	(assert (k-cell (x 1) (y ?c) (content water)))
	(assert (k-cell (x 2) (y ?c) (content water)))
	(assert (k-cell (x 3) (y ?c) (content water)))
	(assert (k-cell (x 4) (y ?c) (content water)))
	(assert (k-cell (x 5) (y ?c) (content water)))
	(assert (k-cell (x 6) (y ?c) (content water)))
	(assert (k-cell (x 7) (y ?c) (content water)))
	(assert (k-cell (x 8) (y ?c) (content water)))
	(assert (k-cell (x 9) (y ?c) (content water)))
	(assert (k-col-water (col ?c)))
	(printout t crlf)
)

; Asserts water in all cells that surround the game table to avoid problems of trespassing
(defrule settWaterBoundary (declare (salience 500))
	(status (step ?s)(currently running))
=>
	(assert (k-cell (x -1) (y 0)))	(assert (k-cell (x -1) (y 1)))	(assert (k-cell (x -1) (y 2)))
	(assert (k-cell (x -1) (y 3)))	(assert (k-cell (x -1) (y 4)))	(assert (k-cell (x -1) (y 5)))
	(assert (k-cell (x -1) (y 6)))	(assert (k-cell (x -1) (y 7)))	(assert (k-cell (x -1) (y 8)))
	(assert (k-cell (x -1) (y 9)))

	(assert (k-cell (x 0) (y -1)))	(assert (k-cell (x 1) (y -1)))	(assert (k-cell (x 2) (y -1)))
	(assert (k-cell (x 3) (y -1)))	(assert (k-cell (x 4) (y -1)))	(assert (k-cell (x 5) (y -1)))
	(assert (k-cell (x 6) (y -1)))	(assert (k-cell (x 7) (y -1)))	(assert (k-cell (x 8) (y -1)))
	(assert (k-cell (x 9) (y -1)))

	(assert (k-cell (x 0) (y 10)))	(assert (k-cell (x 1) (y 10)))	(assert (k-cell (x 2) (y 10)))
	(assert (k-cell (x 3) (y 10)))	(assert (k-cell (x 4) (y 10)))	(assert (k-cell (x 5) (y 10)))
	(assert (k-cell (x 6) (y 10)))	(assert (k-cell (x 7) (y 10)))	(assert (k-cell (x 8) (y 10)))
	(assert (k-cell (x 9) (y 10)))

	(assert (k-cell (x 10) (y 0)))	(assert (k-cell (x 10) (y 1)))	(assert (k-cell (x 10) (y 2)))
	(assert (k-cell (x 10) (y 3)))	(assert (k-cell (x 10) (y 4)))	(assert (k-cell (x 10) (y 5)))
	(assert (k-cell (x 10) (y 6)))	(assert (k-cell (x 10) (y 7)))	(assert (k-cell (x 10) (y 8)))
	(assert (k-cell (x 10) (y 9)))
)

; ######################## MANAGEMENT OF SHIPS FOUND ########################

; Vertical battleships
(defrule verticalBattleshipFound
	(status (step ?s)(currently running))
	?btf <- (battleship (to_find ?to_find_b ))
	(battleship (to_find ?to_find_b &:(> ?to_find_b 0)))
	(or			
		; If middle cell is guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed)) 
		(cell_status (kx ?x) (ky ?y) (stat fired)) 
	)
	(or			
		; If top cell is guessed or fired
		(cell_status (kx ?x_top &:(eq ?x_top (- ?x 1))) (ky ?y) (stat guessed))
		(cell_status (kx ?x_top &:(eq ?x_top (- ?x 1))) (ky ?y) (stat fired))
	)	
	(or			
		; If middle cell is guessed or fired
		(cell_status (kx ?x_mid &:(eq ?x_mid (+ ?x 1))) (ky ?y) (stat guessed))	
		(cell_status (kx ?x_mid &:(eq ?x_mid (+ ?x 1))) (ky ?y) (stat fired))
	)
	(or			
		; If bottom cell is guessed or fired
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 2))) (ky ?y) (stat guessed))	
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 2))) (ky ?y) (stat fired))
	)
	; Check that you have water at the ends
	(k-cell (x ?x_top2 &:(eq ?x_top2 (- ?x 2))) (y ?y) (content water)) 
	(k-cell (x ?x_bot2 &:(eq ?x_bot2 (+ ?x 3))) (y ?y) (content water)) 
	; Check that it has not already been found
	(not (battleship_vert_found 
		(xtop ?xtop &:(eq ?xtop (- ?x 1)))
		(xmid ?x)
		(xmid1 ?xmid &:(eq ?xmid (+ ?x 1))) 
		(xbot ?xbot &:(eq ?xbot (+ ?x 2))) 
		)
	)
=>	
	(modify ?btf (to_find (- ?to_find_b 1)))
	(assert (battleship_vert_found (xtop (- ?x 1))	(xmid ?x) (xmid1 (+ ?x 1)) (xbot (+ ?x 2))))
	(printout t crlf)
	(printout t "VERTICAL BATTLESHIP FOUND!")
	(printout t crlf)
)

; Horizontal battleships
(defrule horizontalBattleshipFound 
	(status (step ?s)(currently running))
	?btf <- (battleship (to_find ?to_find_b ))
	(battleship (to_find ?to_find_b &:(> ?to_find_b 0)))
	(or			
		; If middle cell is guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed)) 
		(cell_status (kx ?x) (ky ?y) (stat fired)) 
	)
	(or			
		; If top cell is guessed or fired
		(cell_status (kx ?x) (ky ?y_left &:(eq ?y_left (- ?y 1))) (stat guessed))
		(cell_status (kx ?x) (ky ?y_left &:(eq ?y_left (- ?y 1))) (stat fired))
	)	
	(or			
		; If middle cell is guessed or fired
		(cell_status (kx ?x) (ky ?y_mid &:(eq ?y_mid (+ ?y 1))) (stat guessed))	
		(cell_status (kx ?x) (ky ?y_mid &:(eq ?y_mid (+ ?y 1))) (stat fired))
	)
	(or			
		; If right cell is guessed or fired
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 2))) (stat guessed))	
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 2))) (stat fired))
	)
	; Check that you have water at the ends
	(k-cell (x ?x) (y ?y_left2 &:(eq ?y_left2 (- ?y 2))) (content water)) 
	(k-cell (x ?x) (y ?y_right2 &:(eq ?y_right2 (+ ?y 3))) (content water)) 
	; Check that it has not already been found
	(not (battleship_orizz_found 
		 (x ?x) (ysx ?yleft &:(eq ?yleft (- ?y 1))) 
		 (ymid ?y) 
		 (ymid1 ?ymid &:(eq ?ymid (+ ?y 1)))
		 (ydx ?yright &:(eq ?yright (+ ?y 2))) 
		)
	) 
=>	
	(modify ?btf (to_find (- ?to_find_b 1)))
	(assert (battleship_orizz_found (x ?x) (ysx (- ?y 1)) (ymid ?y) (ymid1 (+ ?y 1)) (ydx (+ ?y 2)))) 
	(printout t crlf)
	(printout t "HORIZONTAL BATTLESHIP FOUND!")
	(printout t crlf)
)

; Vertical cruisers
(defrule verticalCruiserFound 
	(status (step ?s)(currently running))
	?ctf <- (cruiser (to_find ?to_find_c ))
	(cruiser (to_find ?to_find_c &:(> ?to_find_c 0)))
	(or			
		; If middle cell is guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed)) 
		(cell_status (kx ?x) (ky ?y) (stat fired)) 
	)
	(or			
		; If top cell is guessed or fired
		(cell_status (kx ?x_top &:(eq ?x_top (- ?x 1))) (ky ?y) (stat guessed))
		(cell_status (kx ?x_top &:(eq ?x_top (- ?x 1))) (ky ?y) (stat fired))
	)	
	(or			
		; If bottom cell is guessed or fired
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 1))) (ky ?y) (stat guessed))	
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 1))) (ky ?y) (stat fired))
	)
	; Check that you have water at the ends
	(k-cell (x ?x_top2 &:(eq ?x_top2 (- ?x 2))) (y ?y) (content water)) 
	(k-cell (x ?x_bot2 &:(eq ?x_bot2 (+ ?x 2))) (y ?y) (content water)) 
	; Check that it has not already been found
	(not (cruiser_vert_found 
		(xtop ?xtop &:(eq ?xtop (- ?x 1)))
		(xmid ?x)
		(xbot ?xbot &:(eq ?xbot (+ ?x 1))) 
		(y ?y)
		)
	)
=>	
	(modify ?ctf (to_find (- ?to_find_c 1)))
	(assert (cruiser_vert_found (xtop (- ?x 1))	(xmid ?x) (xbot (+ ?x 1)) (y ?y)))
	(printout t crlf)
	(printout t "VERTICAL CRUISER FOUND!")
	(printout t crlf)
)

; Horizontal cruisers
(defrule horizontalCruiserFound 
	(status (step ?s)(currently running))
	?ctf <- (cruiser (to_find ?to_find_c ))
	(cruiser (to_find ?to_find_c &:(> ?to_find_c 0)))
	(or			
		; If middle cell is guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed)) 
		(cell_status (kx ?x) (ky ?y) (stat fired)) 
	)
	(or			
		; If top cell is guessed or fired
		(cell_status (kx ?x) (ky ?y_left &:(eq ?y_left (- ?y 1))) (stat guessed))
		(cell_status (kx ?x) (ky ?y_left &:(eq ?y_left (- ?y 1))) (stat fired))
	)	
	(or			
		; If right cell is guessed or fired
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 1))) (stat guessed))	
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 1))) (stat fired))
	)
	; Check that you have water at the ends
	(k-cell (x ?x) (y ?y_left2 &:(eq ?y_left2 (- ?y 2))) (content water)) 
	(k-cell (x ?x) (y ?y_right2 &:(eq ?y_right2 (+ ?y 2))) (content water)) 
	; Check that it has not already been found
	(not (cruiser_orizz_found 
		 (x ?x) (ysx ?yleft &:(eq ?yleft (- ?y 1))) 
		 (ymid ?y) 
		 (ydx ?yright &:(eq ?yright (+ ?y 1))) 
		)
	) 
=>	
	(modify ?ctf (to_find (- ?to_find_c 1)))
	(assert (cruiser_orizz_found (x ?x) (ysx (- ?y 1)) (ymid ?y) (ydx (+ ?y 1)))) 
	(printout t crlf)
	(printout t "HORIZONTAL CRUISER FOUND!")
	(printout t crlf)
)

; Vertical destroyers
(defrule verticalDestroyerFound 
	(status (step ?s)(currently running))
	?ctf <- (destroyer (to_find ?to_find_c ))
	(destroyer (to_find ?to_find_c &:(> ?to_find_c 0)))
	
	(or			
		; If top cell is guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed)) 
		(cell_status (kx ?x) (ky ?y) (stat fired)) 
	)
	(or			
		; If bottom cell is guessed or fired
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 1))) (ky ?y) (stat guessed))	
		(cell_status (kx ?x_bot &:(eq ?x_bot (+ ?x 1))) (ky ?y) (stat fired))
	)
	; Check that you have water at the ends
	(k-cell (x ?x_top2 &:(eq ?x_top2 (- ?x 1))) (y ?y) (content water)) 
	(k-cell (x ?x_bot2 &:(eq ?x_bot2 (+ ?x 2))) (y ?y) (content water)) 
	; Check that it has not already been found
	(not (destroyer_vert_found (xtop ?x) (xbot ?xbot &:(eq ?xbot (+ ?x 1))) (y ?y)))
=>	
	(modify ?ctf (to_find (- ?to_find_c 1)))
	(assert (destroyer_vert_found (xtop  ?x ) (xbot (+ ?x 1)) (y ?y)))
	(printout t crlf)
	(printout t "VERTICAL DESTROYER FOUND!")
	(printout t crlf)
)

; Horizontal destroyers
(defrule horizontalDestroyerFound 
	(status (step ?s)(currently running))
	?ctf <- (destroyer (to_find ?to_find_c ))
	(destroyer (to_find ?to_find_c &:(> ?to_find_c 0)))
	(or			
		; Se left e' guessed or fired
		(cell_status (kx ?x) (ky ?y) (stat guessed)) 
		(cell_status (kx ?x) (ky ?y) (stat fired)) 
	)
	(or			
		; If right cell is guessed or fired
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 1))) (stat guessed))	
		(cell_status (kx ?x) (ky ?y_right &:(eq ?y_right (+ ?y 1))) (stat fired))
	)
	; Check that you have water at the ends
	(k-cell (x ?x) (y ?y_left2 &:(eq ?y_left2 (- ?y 1))) (content water)) 
	(k-cell (x ?x) (y ?y_right2 &:(eq ?y_right2 (+ ?y 2))) (content water)) 
	; Check that it has not already been found
	(not (destroyer_orizz_found (x ?x) (ysx ?y) (ydx ?yright &:(eq ?yright (+ ?y 1))))) 
=>	
	(modify ?ctf (to_find (- ?to_find_c 1)))
	(assert (destroyer_orizz_found (x ?x) (ysx  ?y ) (ydx (+ ?y 1)))) 
	(printout t crlf)
	(printout t "HORIZONTAL DESTROYER FOUND!")
	(printout t crlf)
)

; ######################## INVOCATION EXTERNAL MODULES ########################

; When it can no longer do anything the focus is passed to the Guess Module
(defrule guessModule 
	(status (step ?s)(currently running))
	(moves (guesses ?ng &:(> ?ng 0)))
	=>
	(printout t crlf)
	(focus GUESS_MOD)
)

; When it doesn't know where to make Guess the focus is passed to the Fire Valutation Module
(defrule fireValModule (declare (salience -300))
	(status (step ?s)(currently running))
	=>
	(focus FIRE_VAL)  
)

; ######################## TERMINATION ########################

; When it doesn't know what to do is invoked the SOLVE
(defrule solve (declare (salience -500))
	(status (step ?s)(currently running))
	(submarine (to_find ?to_find_s))
	(destroyer (to_find ?to_find_d))
	(cruiser (to_find ?to_find_c))
	(battleship (to_find ?to_find_b))
	(moves (fires ?nf) (guesses ?ng))
=>
	(assert (exec (step ?s) (action solve)))
	(printout t crlf)
	(printout t "I don't know what else to do. I'm ending the game." crlf)
	(printout t crlf)
	(printout t "Fires left: " ?nf ", Guess left: " ?ng " "crlf)
	(printout t crlf)
	(pop-focus)
)