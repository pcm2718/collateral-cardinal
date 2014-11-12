;; Move Structure and Board Operations using Moves (things which
;; understand moves)
;; ===============================

;;(defmacro neq (&rest args)
;;  `(not (eq ,args)))

;; TODO Make this better defined with no and a single argument.
(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

(defun piece-team (piece)
  (cond
    ((member piece '(rm rk)) 'r)
    ((member piece '(bm bk)) 'b)
    (t nil)))

(defun enemiesp (one two)
  (not (eq (piece-team one) (piece-team two))))

(defun jump-piece-dir-validate (orig dest piece)
  ;; Test true if piece is a king, or is a man moving in his
  ;; respective forward. A bit ugly, as the directions for red and
  ;; black are hardcoded.
  (or (member piece '(rk bk))
	  (and (eq piece 'rm) (plusp (- (second dest) (second orig))))
	  (and (eq piece 'bk) (minusp (- (second dest) (second orig))))))

;; Is there a way to rewrite this as destructive while improving
;; performance?
;; TODO Lots of redundant computation happens here.
(defun jump-board-validate (orig dest board)
  (let ((piece (board-spot-get board orig)))
    ;; Test true if orig is occupied, dest is unoccupied, and the
    ;; direction the piece is traveling is valid.
    (if (and piece
	     (not (board-spot-get board dest))
	     (jump-piece-dir-validate orig dest piece))

	;; If the test is true, we can procede to determine whether
	;; the the jump was simple, a capture, or invalid. This
	;; recomputes some values from jump-piece-dir-validate, maybe
	;; they could be combined for efficiency reasons.
	;; Clean this up to remove redundant computation
	(cond 
	  ;; If the piece is jumping diagonally by one square, then
	  ;; the jump is simple.
	  ((= 1
	      (abs (- (first dest) (first orig)))
	      (abs (- (second dest) (second orig))))
	   'simple)

	  ;; If the piece is jumping diagonally by two squares and an
	  ;; enemy piece lies in-between the two squares, then the
	  ;; jump is a capture.
	  ;; TODO Clean this up to remove redundant computation. Maybe
	  ;; avg should work multidimensionally?
	  ((and (= 2
		   (abs (- (first dest) (first orig)))
		   (abs (- (second dest) (second orig))))
		(enemiesp piece (board-spot-get board
						(list (avg (first dest)
							   (first orig))
						      (avg (second dest)
							   (second orig))))))
	   'capture)

	  ;; If the piece is moving neither one nor two squares
	  ;; diagonally, it is invalid, so return nil.
	  (t nil))

	;; If the test is false we know the jump is invalid, so return nil.
	nil)))

;; TODO Reduce redundant and unneccesary computation. Annotation?
(defun jump-board-applyf (orig dest board)
  (if (= 2 (abs (- (first dest) (first orig))))
      (board-spot-setf board
		       (list (avg (first dest) (first orig))
			     (avg (second dest) (second orig)))
		       nil))
  (board-spot-swapf board orig dest))

;; TODO Maybe jump-board-validate also functions as an applicator?
;; TODO Write a function to efficiently determine the relation between
;; a number and the length of a list.
;; TODO Could validate also splice extra information into the move to
;; speed up the application of a move to a board and avoid redundant
;; computation (aka store information about captured pieces so the
;; applicator can avoid recomputing captures)? I would have enable the
;; applicator to read either the modified or unmodified move to retain
;; the functionality I want.
;; TODO Could move-board-validate be merged with move-board-validate-r?
;; TODO Forget annotation, this function computes the result board as
;; a consequence of verification. This function perhaps should return
;; nil if invalid, else the result board. the move-board-apply
;; function could be reserved for a function which does not do
;; validation during application.

;; Try to apply the first jump in move to board. If successful
;; recursively try to apply the remainder of the jumps to board and
;; return the board, else return nil.
;; TODO Reimplement with cond?
(defun move-board-apply-r (move board)
  ;; Test to determine whether the last jump has been tested.
  (if (eq (cdr move) ())
      ;; If so, then return the result board.
      board
      ;; Else, validate the current jump and recursively validate the
      ;; remaining jumps. From and to of move are accessed twice.
      (if (jump-board-validate (car move) (cadr move) board)
	  (progn (jump-board-applyf (car move) (cadr move) board)
		 (move-board-apply-r (cdr move) board))
	  nil)))

;; If possbile apply the supplied move to a copy of board and return
;; the copy, else return nil.
(defun move-board-apply (move board)
  ;; Test to determine whether move contains at least one jump.
  ;; This test is redundant on the first level of the recursive call.
  (if (eq (cdr move) ())
      ;; If not, then move isn't really a move, so return nil.
      nil
      ;; Else, copy the board and use the copy to try and apply the
      ;; move.
      (move-board-apply-r move (board-deep-copy board))))
