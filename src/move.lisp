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

;; TODO Lots of redundant computation happens here.
(defun jump-board-validate (orig dest board)
  (let ((piece (board-spot-get orig)))
    ;; Test true if orig is occupied, dest is unoccupied, and the
    ;; direction the piece is traveling is valid.
    (if (and piece
	     (not (board-spot-get dest))
	     (jump-piece-dir-validate oirg dest piece))

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
		(enemiesp piece (board-spot-get (list (avg (first dest)
							   (first orig))
						      (avg (second dest)
							   (second orig))))))
	   'capture)

	  ;; If the piece is moving neither one nor two squares
	  ;; diagonally, it is invalid, so return nil.
	  (t nil))

	;; If the test is false we know the jump is invalid, so return nil.
	nil)))

;;(defun jump-board-applyf (orig dest board))

(defun jump-board-apply (orig dest board)
  (board-spot-swap board orig dest))

(defun move-board-validate-r (move board)
  ;; Test to determine whether the last jump has been tested.
  (if (eq (cddr move) ())
      ;; If so, then return true.
      t
      ;; Else, validate the current jump and recursively validate the
      ;; remaining jumps.
      (and (jump-board-validate (car move) (cadr move) board)
	   ((move-board-validate ((cdr move) (jump-board-apply board)))))))

;; TODO Write a function to efficiently determine the relation between
;; a number and the length of a list.
(defun move-board-validate (move board)
  ;; Test to determine whether move contains at least one jump.
  (if (eq (cdr move) ())
      ;; If not, then move isn't really a move, so return nil.
      nil
      ;; Else, validate the move.
      (move-board-validate-r move board)))

;; Apply the supplied move to the given board. Unsafe, destructive.
(defun %move-board-apply (move board)
  (let ((origin (move-origin move))
	(dest (move-dest move)))
    (board-spot-setf board dest (board-spot-get board origin))
    (board-spot-setf board origin 'empty)
    T))

;; Apply the supplied move to the given board, with verification. Destructive.
(defun move-board-applyf (move board)
  (if
   (move-board-validate move board)
   (%move-board-apply move board)
   '()))

;; Apply the supplied move to a deep copy of the given board and return it.
(defun move-board-apply (move board)
  (let
      ((deep-copy (board-deep-copy board)))
    (move-board-applyf move deep-copy)
    deep-copy))
