(in-package :academy)

;;; !!! Attention Hackers. Get your fork on to make the
;;;
;;;     Hall Of Programmers

(defun list-all-programmers ()
  (sort
   (count-unique (split-string-into-lines
                  (git '("log" "--format=format:%an"))))
   #'> :key #'second))

(defun hall-of-programmers ()
  "Show the Hall of Programmers."
  (print-table (list-all-programmers)))
