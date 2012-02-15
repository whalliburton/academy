(in-package :academy)

(defun list-files (directory mask)
  (sort (directory (format nil "~A~A" directory mask)) #'string<
        :key #'pathname-name))

(defun choose-file (directory mask &key (prompt "Choose a file:") default)
  (read-choice
   (loop for i from 1
         for pgn in (list-files directory mask)
         collect (list (substitute #\space #\- (pathname-name pgn)) pgn))
   :prompt prompt
   :default default))
