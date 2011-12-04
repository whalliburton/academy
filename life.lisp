(in-package :academy)

(defparameter *life-grid-width* 32)
(defparameter *life-grid* nil)

(defun make-life-grid ()
  (make-bitmap *life-grid-width* *life-grid-width*))

(defun clear-life-grid ()
  (setf *life-grid* (make-life-grid)))

(defun age-life-grid ()
  (let ((next-generation (make-life-grid)))
    (loop for y from 0 to (1- *life-grid-width*)
          do (loop for x from 0 to (1- *life-grid-width*)
                   do (let ((neighbors
                              (let ((total 0))
                                (loop for (dx dy) in '((-1 -1) (0 -1) (1 -1)
                                                       (-1 0)         (1 0)
                                                       (-1 1)  (0 1)  (1 1))
                                      do (let ((xi (+ dx x))
                                               (yi (+ dy y)))
                                           (unless (or (minusp xi) (minusp yi)
                                                       (= xi *life-grid-width*)
                                                       (= yi *life-grid-width*))
                                             (when (aref *life-grid* yi xi)
                                               (incf total)))))
                                total)))
                        (setf (aref next-generation y x)
                              (if (aref *life-grid* y x) ; we got a live one
                                (cond
                                  ((< neighbors 2) nil)  ; lonliness
                                  ((<= neighbors 3) t)   ; party on dude!
                                  ((> neighbors 3) nil)) ; overcrowding
                                (if (= neighbors 3) ; kinky!
                                  t
                                  nil))))))
    (setf *life-grid* next-generation)))

(defun life (&key (pattern '("  ***"
                             " *  *"
                             "*   *"))
                  (steps 32))
  "Play Conway's Game of Life."
  (clear-life-grid)
  (center-on-bitmap *life-grid* pattern)
  (draw *life-grid*)
  (dotimes (x steps)
    (age-life-grid)
    (draw *life-grid*)))
