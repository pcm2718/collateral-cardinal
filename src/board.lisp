;; Mathematical Primitives
;; ===============================

;; Determine whether value is in the open set represented by range.
(defun in-open-range (value range)
  (and (> value (first range)) (< value (second range))))

;; Determine whether value is in the closed set represented by range.
(defun in-closed-range (value range)
  (and (>= value (first range)) (<= value (second range))))



;; Board Structure and Primitives (things which understand pieces)
;; ===============================

;; This structure functions as an abstraction separating the game
;; board from the implementation.
(defstruct board board)

;; Pretty-print the board in a way that makes sense to a human.
;; TODO Modify this to handle boards of any size, with improved
;; efficency.
(defun board-pprint (board)
  (let
      ((dim (board-dim board)))
    (dotimes (x 8 nil)
      (dotimes (y 8 nil)
	(prin1 (board-spot-get board (list x y))))
      (prin1 ""))))

;; TODO Implement this later.
;;(defun board-pprint-color (board))

;; Build a square board of dimension dim.
(defmacro build-board (dim)
  `(make-board :board (make-array '(,dim ,dim) :initial-element nil)))

;; Get the dimension of the board.
(defmacro board-dim (board)
  `(array-dimension (board-board ,board) 0))

;; Function providing bounds checking for a spot.
(defun board-spot-check (board spot)
  (let
      ((indexrange (list 0 (- (board-dim board) 1))))
    (if
     (and (in-closed-range (first spot) indexrange)
	  (in-closed-range (second spot) indexrange))
     T
     '())))

;; Macro providing a reference to a spot on the board.
(defmacro %board-spot-ref (board spot)
  `(apply #'aref (append (list (board-board ,board)) ,spot)))

;; Get the piece type at spot.
(defun board-spot-get (board spot)
  (if
   (board-spot-check board spot)
   (%board-spot-ref board spot)
   '()))

;; Set the piece type at spot destructively.
(defun board-spot-setf (board spot newpiece)
  (if
   (board-spot-check board spot)
   (setf (%board-spot-ref board spot) newpiece)
   '()))

;; Return a shallow copy of an array. Code courtesy of
;; http://jtra.cz/stuff/lisp/sclr/copy-seq.html .
(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))

;; Return a deep copy of the given board.
(defun board-deep-copy (board)
  (make-board :board (copy-array (board-board board))))

;; Set the piece type on a copy of board at spot and return the copy.
(defun board-spot-set (board spot newpiece)
  (let
      ((deep-copy (board-deep-copy board)))
    (board-spot-setf deep-copy spot newpiece)
    deep-copy))

;; Swap the symbols at the two board locations and return the copy.
(defun board-spot-swap (board one two)
  (board-spot-setf (board-spot-set board
				   one
				   (board-spot-get board two))
		   two
		   (board-spot-get board one)))
