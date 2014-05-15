(defstruct player color get-move)



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

        (let
          ;; Find the new state of the game.
          (new-state

            (if
              ;; Test for the existance of possible moves. Might use let.
              (eq current-player-moves '())

              ;; If the player has no possible moves, the state of the game remains unchanged.
              state

              ;; Otherwise, apply the player's chosen move. Might try to take advantage of precomputed moves.
              ;;(apply-move state ((player-get-move current-player) state (player-color current-player)))))
              state

          ;; Find the new player queue.
          (new-players

            (if
              ;; Test for the existance of possible moves. Might use let.
              (eq current-player-moves '())

              ;; If the player has no possible moves, he is removed from the play queue.
              ;; (circular-queue-remove-front players)
              (circular-queue-remove players current-player)

              ;; Otherwise, advance the queue.
              (circular-queue-advance players)))

          ;; Advance the game by making a recursive call.
          (play-game new-state new-players))))))
