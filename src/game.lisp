;; Player Structure and Operations (things which understand players)
;; ===============================

;; This structure associates a piece color with a move selection
;; algorithms. The lack of default values is intentional.
;; TODO Add a saftey indicator to tell the game loop if moves should
;; be verified? This would improve the efficiency of safe mode.
(defstruct player id man king move-selector)

;; Macro to simplify constructing players.
(defmacro build-player (id man king move-selector)
  `(make-player :id ',id :man ',man :king ',king :selector ',move-selector))

(defmacro player-saftey (player)
  `(make-player :id ,(player-id player)
		:man ,(player-man player)
		:king ,(player-king player)
		:selector `(



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

;; Pretty-printer for the state of a game, primarily intended for
;; debugging inspection and user interaction.
(defun state-pprint (state)
  (board-pprint (state-board state)) (write-char #\newline)
  (pprint (state-players state)) (write-char #\newline) nil)



;; Game Loop (this is it, the big one)
;; ===============================

;; The circular queue macros are prerequisite to the game loop.
;;(load "circular-queue")

;; Iterate state by appling moves from the current player until only
;; one player remains.
;; TODO Implement a proper draw system, where the opposing player must
;; agree to the draw.
(defun state-play-game (state)
  ;;(print state)
  ;;(sleep 3)
  (let*
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
      (let
	  ;; Get the player's chosen move, based on the state.
	  ((current-player-move (funcall (player-move-selector current-player)
					 state)))

	;; TODO Add a form to print something like "<player-name> has
	;; made the move: <move>".

	;; TODO Add forms to print status information.

	(if
	 ;; Test to determine whether the player has chosen a move.
	 (or (eq current-player-move nil) (eq current-player-move 'draw))
	 
	 ;; If the player is not left with a legal move, or if he
	 ;; has no pieces, the board remains unchanged and the
	 ;; current player is removed from the queue.
	 (make-state :board board
		     :players (circular-queue-pop players))

	 ;; Otherwise, the player's chosen move is applied to the
	 ;; board while the player queue is advanced.

	 (make-state :board (move-board-apply current-player-move board)
		     :players (circular-queue-advance players))))))))

;; (defmacro state-play-game-safe (state)
;;   `(state-play-game (make-state :board (state-board state) :players
