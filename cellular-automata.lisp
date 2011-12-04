(in-package :academy)

(defparameter *rule-patterns*
  '((nil nil nil) (nil nil t) (nil t nil) (nil t t) (t nil nil) (t nil t) (t t nil) (t t t)))

(defun pick-rules (rule)
  (loop for row in *rule-patterns*
        for index from 0
        when (logbitp index rule)
        collect row))

(defun draw-cellular-automata (&optional (rule 30) (size 64))
  "Draw an elementary cellular automata."
  (let ((rules (pick-rules rule)))
    (let ((bitmap (make-bitmap size size)))
      (setf (aref bitmap 0 (floor size 2)) t)
      (loop for y from 1 to (1- size)
            do (loop for x from 1 to (- size 2)
                     do (when (find (list
                                     (aref bitmap (1- y) (1- x))
                                     (aref bitmap (1- y) x)
                                     (aref bitmap (1- y) (1+ x)))
                                    rules :test #'equalp)
                          (setf (aref bitmap y x) t))))
      (draw bitmap))))
