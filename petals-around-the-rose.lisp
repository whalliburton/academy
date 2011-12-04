(in-package :academy)

(defun roll-die ()
  (let ((index (random 6)))
    (values
     (nth index
          '(#\DIE_FACE-1
            #\DIE_FACE-2
            #\DIE_FACE-3
            #\DIE_FACE-4
            #\DIE_FACE-5
            #\DIE_FACE-6))
     (if (evenp (1+ index)) 0 index))))

(defun petals-around-the-rose ()
  "Play the petals around the rose dice game. Guess the rules."
  (let ((total 0))
    (dotimes (i 6)
      (multiple-value-bind (face score) (roll-die)
        (incf total score)
        (princ face)
        (princ #\space)))
    (fresh-line)
    total))
