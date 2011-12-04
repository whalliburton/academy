(in-package :academy)

(defun visualize-one-in-chance (&optional (chance 3))
  "Show a 32x32 bitmap with pixels on with a one in CHANCE probability."
  (draw-bitmap-from-list (random-booleans (* 32 32) chance) 32))
