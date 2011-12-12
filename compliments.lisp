(in-package :academy)

;;; Attention Hackers! Exercises are good for the heart!
;;;
;;;    Add some love.

(defun compliment ()
  "Warm thy soul."
  (format t "You are so ~A.~%"
          (random-element
           '("beautiful" "smart" "eloquent" "intelligent" "discerning"))))
