(in-package :academy)

(defparameter *life-grid-width* 32)
(defparameter *life-grid* nil)

(defun make-life-grid ()
  (make-array (list *life-grid-width* *life-grid-width*) :initial-element nil))

(defun clear-life-grid ()
  (setf *life-grid* (make-life-grid)))

(defun draw-life-grid ()
  (loop for y from 0 to (1- *life-grid-width*) by 2
        do (loop for x from 0 to (1- *life-grid-width*)
                 do (princ
                     (let ((top (aref *life-grid* x y))
                           (bottom (aref *life-grid* x (1+ y))))
                       (cond
                         ((and top bottom) #\FULL_BLOCK)
                         (top              #\UPPER_HALF_BLOCK)
                         (bottom           #\LOWER_HALF_BLOCK )
                         (t                #\space)))))
           (fresh-line)))

(defun copy-onto-grid (pattern x y)
  (loop for row in pattern
        for yi from y
        do (loop for character in (coerce row 'list)
                 for xi from x
                 do (setf (aref *life-grid* xi yi) (not (eq character #\space))))))

(defun center-on-grid (pattern)
  (copy-onto-grid pattern
                  (- (floor *life-grid-width* 2) (floor (length (car pattern)) 2))
                  (- (floor *life-grid-width* 2) (floor (length pattern) 2))))

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
                                             (when (aref *life-grid* xi yi)
                                               (incf total)))))
                                total)))
                        (setf (aref next-generation x y)
                              (if (aref *life-grid* x y) ; we got a live one
                                (cond
                                  ((< neighbors 2) nil)  ; lonliness
                                  ((<= neighbors 3) t)   ; party on dude!
                                  ((> neighbors 3) nil)) ; overcrowding
                                (when (= neighbors 3) ; kinky!
                                  t))))))
    (setf *life-grid* next-generation)))

(defun life (&key (pattern '("  ***"
                             " *  *"
                             "*   *"))
                  (steps 32))
  "Play Conway's Game of Life."
  (clear-life-grid)
  (center-on-grid pattern)
  (draw-life-grid)
  (dotimes (x steps)
    (age-life-grid)
    (draw-life-grid)))
