(in-package :academy)

(defun print-log ()
  (princ (git '("log" "--reverse" "--format=format:%s%n%n%b%n---%n")))
  nil)

(defun save-log ()
  "Save the entire log to a file for reading."
  (with-output-to-data-file "log.txt"
    (print-log)))
