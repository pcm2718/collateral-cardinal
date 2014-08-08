;; Circular Queue Macros
;; ===============================

;; Create a circular queue, binding it to queue.
(defmacro circular-queue-create (queue)
  `(setf ,queue ()))

;; Get the item at the front of the queue.
(defmacro circular-queue-front (queue)
  `(car ,queue))

;; Push an item onto the back of the queue without evaluating the
;; item.
(defmacro circular-queue-push (queue item)
  `(append ,queue (list ,item)))

;; Pop the item on the front of the queue.
(defmacro circular-queue-pop (queue)
  `(cdr ,queue))

;; Advance the circular queue by moving the front element to the back.
(defmacro circular-queue-advance (queue)
  `(circular-queue-pop (append ,queue (list (circular-queue-front ,queue)))))
