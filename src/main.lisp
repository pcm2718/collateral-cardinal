;; Circular List Macros
;; ===============================
;; Double check these to make sure they are non-destructive.

;; Create a circular queue, binding it to queue.
(defmacro circular-queue-create (queue)
  `(setf ,queue '()))

;; Get the item at the front of the queue.
(defmacro circular-queue-front (queue)
  `(car ,queue))

;; Push an item onto the back of the queue.
(defmacro circular-queue-push (queue item)
  `(setf ,queue (append ,queue (list ,item))))

;; Pop the item on the front of the queue.
(defmacro circular-queue-pop (queue)
  `(setf ,queue (cdr ,queue)))

;; Advance the circular queue by moving the front element to the back.
(defmacro circular-queue-advance (queue)
  `(circular-queue-pop (circurlar-queue-push (,queue (circular-queue-front ,queue)))))





;; State Structure and Operations
;; ===============================

;; Read the state from instream.
(defun state-read (state instream)
  ())



;; Write the state to outstream.
(defun state-write (state outstream)
  ())



;; Get possible moves within state by pieces of the given color.
(defun state-moves (state color)
  ())





;; Move Selectors
;; ===============================

;; Select a random move from the list of possible moves.
(defun select-move-rand (state colour)
  ())



;; Use MCTS to select a move from the list of possible moves.
(defun select-move-mcts-serial (state color budget)
  ())



;; Use parallel MCTS to select a move from the list of possible moves.
(defun select-move-mcts-parallel (state color budget)
  ())





;; Checkers Playthrough Algorithm
;; ===============================

;; This structure associates a piece color with a move selection algorithms.
;;;; Named by analogy with a person standing in a queue.
(defstruct player color select-move)



;; Algorithm for playing the checkers game through to the end and returning the winning player.
(defun play-game (state players)
  (let
    ;; Find the current player.
    (current-player (circular-queue-front players))

    (if
      ;; Test to see if only one player remains.
      ;; (eq (length players) 1)
      (eq (cdr players) '())

      ;; If only one player remains, return the player.
      (circular-queue-front players)

      ;; Otherwise, activate the recursive step.
      (let
        ;; Find the current player's options for movement.
        (current-player-moves (get-moves state (player-color current-player)))

        ;; I might be able to use a macro here to simplify the pair of if statements.
        (let
          ;; Find the new state of the game.
          (new-state

            (if
              ;; Test for the existence of possible moves. Might use let.
              (eq current-player-moves '())

              ;; If the player has no possible moves, the state of the game remains unchanged.
              state

              ;; Otherwise, apply the player's chosen move. Might try to take advantage of precomputed moves.
              ;; This bit needs to be fixed.
              (apply-move state ((player-select-move current-player) state (player-color current-player)))

              ;; Find the new player queue.
              (new-players

                (if
                  ;; Test for the existence of possible moves. Might use let.
                  (eq current-player-moves '())

                  ;; If the player has no possible moves, he is removed from the play queue.
                  ;; (circular-queue-remove players current-player)
                  (circular-queue-remove-front players)

                  ;; Otherwise, just advance the queue.
                  (circular-queue-advance players)))

              ;; Advance the game by making a recursive call.
              (play-game new-state new-players))))))))





;; Top Level
;; ===============================
;; Might excise this to a sort of script later.

;; Create the state.
;; Load the state from init file.

;; Create the player queue.
(circular-queue-create players)
;; Add the red and black players, random and serial MCTS respectively, MCTS budget of 1000 milliseconds.
(circular-queue-push (player :color 'red :get-move 'select-move-rand))
(circular-queue-push (player :color 'black :get-move 'select-move-mcts-serial))

;; ROUND 1, FIGHT!
(print (play-game state players))
