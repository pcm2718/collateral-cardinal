;; Player Structure and Operations (things which understand players)
;; ===============================

;; This structure associates a piece color with a move selection
;; algorithms. The lack of default values is intentional.
(defstruct player man king move-selector)

;; Macro to simplify constructing players.
(defmacro build-player (man king move-selector)
  `(make-player :man ',man :king ',king :move-selector ',move-selector))



;; State Structure and Operations (things which understand state)
;; ===============================

;; This structure defines the state of a game.
(defstruct state 
  (board (make-board))
  (players '()))

;; Macro to simplify constructing the state.
(defmacro build-state (board players)
  `(make-state :board ',board :players ',players))

;; Macro to simplify constructing the state.
(defmacro build-state-list (proplist)
  `(eval (append '(build-state) ,proplist)))

;; Get the dimension of the state's board.
;;(defmacro state-dim (state)
;;  `(board-dim (state-board ,state)))

;; Iterate state by appling moves from the current player until only
;; one player remains.
(defun state-play-game (state)
  ;;(print state)
  ;;(sleep 3)
  (let
      (
       ;; Bind the board.
       (board (state-board state))

       ;; Bind the player queue.
       (players (state-players state))

       ;; Find the current player.
       (current-player (circular-queue-front players)))

    (if
     ;; Test to see if only one player remains.
     (eq (cdr players) '())

     ;; If only one player remains, it must be the winning player,
     ;; and thus returned.
     current-player

     ;; Otherwise, activate the recursive step using the next state
     ;; of the game as the argument.
     (state-play-game
      (build-state-list
       (let
	   ;; Get the player's chosen move, based on the state.
	   ((current-player-move (funcall (player-select-move current-player)
					  state)))
	 (if
	  ;; Test to determine whether the player has chosen a move.
	  (eq current-player-move '())
	  
	  ;; If the player is not left with a legal move, or if he
	  ;; has no pieces, the board remains unchanged and the
	  ;; current player is removed from the queue.
	  (list board
		(circular-queue-pop players))

	  ;; Otherwise, the player's chosen move is applied to the
	  ;; board while the player queue is advanced.

	  (list (move-board-apply current-player-move board)
		(circular-queue-advance players)))))))))
