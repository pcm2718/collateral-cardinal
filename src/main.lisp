;; Player Structure and Operations, including Move Selectors (things
;; which understand colors and players)
;; ===============================

;; This structure associates a piece color with a move selection
;; algorithms.  The lack of default values is intentional, for now.
(defstruct player man king select-move)

;; Macro to simplify constructing players.
(defmacro build-player (man king move-selector)
  `(make-player :man ',man :king ',king :move-selector ',move-selector))



(load "board")
(load "move")
(load "circular-queue")
(load "state")
(load "move-selectors")



;; Top Level
;; ===============================

;; There is clearly a better way to organize the loops in this. Use
;; some macros to keep the parity from being evaluated constantly?
;; Reimplement with do?
(defun build-standard-board ()
  (let
      ((board (build-board 8)))
    ;; Put down red pieces.
    (dotimes (y 3 nil)
      (dotimes (x 4 nil)
	(board-spot-setf board (list (+ (* x 2) (if (oddp y) 1 0)) y) 'rm)))
    
    ;; Put down black pieces (and return the board?).
    (dotimes (y 3 nil)
      (dotimes (x 4 nil)
	(board-spot-setf board (list (+ (* x 2) (if (oddp (+ y 5)) 1 0)) (+ y 5)) 'bm)))

    ;; Return the board.
    board))

;; Create a standard, initialized playing board and shove it in
;; myboard.
(setf myboard (build-standard-board))

;; Create the player queue.
(circular-queue-create myplayers)

;; Add the red and black players, random and serial MCTS respectively, MCTS budget of 1000 milliseconds.
;;(circular-queue-push players (struct-player :color 'red :get-move 'select-move-rand))
;;(circular-queue-push players (struct-player :color 'black :get-move 'select-move-mcts-serial))

;; Add the read and black players, both human.
(setf myplayers (circular-queue-push myplayers
				     (build-player 'rm 'rk 'select-move-human)))
(setf myplayers (circular-queue-push myplayers
				     (build-player 'bm 'bk 'select-move-human)))

;; Build the state.
;; Fix this to work *properly* with symbols later.
;;(setf mystate (build-state myboard myplayers))
(setf mystate (make-state :board myboard :players myplayers))

;; ROUND 1, FIGHT!
(print (state-play-game mystate))
