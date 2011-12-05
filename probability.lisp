(in-package :academy)

(defun visualize-one-in-chance (&optional (chance 3))
  "Show a 32x32 bitmap with pixels on with a one in CHANCE probability."
  (draw-from-list (random-booleans (* 32 32) chance) 32))

(defun visualize-chance (&optional (steps 80))
  "Show a bitmap with each columns pixels with decreasing probability."
  (draw (make-bitmap steps 32
                     (loop for y from 1 to 32
                           collect (loop for i from 1 to steps
                                         collect (>= (random steps) i))))))
