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
  `(setf ,queue (cdr ,queue))

;; Advance the circular queue by moving the front element to the back.
(defmacro advance-circular-queue (queue)
  `(circular-queue-pop (circurlar-queue-push (,queue (circular-queue-front ,queue)))))
