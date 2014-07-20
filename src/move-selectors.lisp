;; Move Generators and Move Selector Algorithms
;; ===============================

;; Generate a list containing all possible moves for a given spot on
;; the board.
(defmacro generate-moves-spot-board (spot board)
  `(let
       ((h (first ,spot))
	(v (second ,spot)))
     (list (list (1+ h) (1+ v)) (list (1+ h) (1- v)) (list (1- h) (1+ v)) (list (1- h) (1- v))
           (list (+ h 2) (+ v 2)) (list (+ h 2) (- v 2)) (list (- h 2) (+ v 2)) (list (- h 2) (- v 2)))))

;; Generate a list containing all possible moves for a piece symbol on
;; the given board, and some that aren't.
(defun generate-moves-piece-board (piece board)
  (let
      ((maxindex (1- (board-dim board))))
    (apply #'append
           (loop for x from 0 to maxindex
	      collect
		(apply #'append (loop for y from 0 to maxindex
				   when
				     (eq (board-spot-get board (list x y)) piece)
				   collect
				     (map 'list
					  #'(lambda (dest) (make-move :origin (list x y) :dest dest))
					  (generate-moves-spot-board (list x y) board))))))))

;; Select a random move from the list of possible moves on state's
;; board for state's current player.
(defun select-move-rand (state)
  (let*
      ((moves (apply #'append (map 'list #'(lambda (piece) (generate-moves-piece-board piece (state-board state))) (player-moveable (circular-queue-front (state-players state))))))
       (move (if (eq (length moves) 0) '() (nth (random (length moves)) moves))))
    (if
     (eq move '())
     '()
     (if
      (move-board-validate move (state-board state))
      move
      (select-move-rand state)))))

;; Use MCTS to select a move from the list of possible moves on
;; state's board for state's current player.
(defun select-move-mcts-serial (state player budget)
  ())

;; Use parallel MCTS to select a move from the list of possible moves
;; on state's board for state's current player.
(defun select-move-mcts-parallel (state color budget)
  ())

;; TODO Add a selector for heuristic shortcut MCTS.

;; Pretty-print the state of the game to the REPL for a human user,
;; then prompt him for a move.
(defun select-move-human (state)
  (progn (board-pprint (state-board state))
	 (prin1 (car (state-players state)))
	 (read)))
