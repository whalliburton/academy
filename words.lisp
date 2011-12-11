(in-package :academy)

(defvar *words* nil)

(defun load-words ()
  (setf *words*
        (coerce (with-open-file (f "/usr/share/dict/words"
                                   :direction :input :if-does-not-exist :error)
                  (loop for line = (read-line f nil)
                        while line
                        collect line))
                'vector))
  (length *words*))
