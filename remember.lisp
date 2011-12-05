(in-package :academy)

(defun remember-data-file ()
  (data-path "remember.txt" :verify nil))

(defun remember (&optional what)
  "Messages to Mnemosyne."
  (let ((filename (remember-data-file)))
    (if what
      (with-open-file (stream filename :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :append)
        (fresh-line stream)
        (princ what stream)
        (fresh-line stream)
        (format t "Noted.~%"))
      (when (probe-file filename)
        (princ (slurp-file filename)))))
  (values))

(defun forget ()
  "Ford the Lethe."
  (with-open-file (stream (remember-data-file)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (format stream "Senses lost ~A seconds after 1/1/1900.~%" (get-universal-time)))
  (format t "But not forgiven.~%"))
