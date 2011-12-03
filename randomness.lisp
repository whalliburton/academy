(in-package :academy)

(defun random-element (sequence)
  (elt sequence (random (length sequence))))
