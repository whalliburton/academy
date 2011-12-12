(in-package :academy)

(defun spellcheck (string)
  "Return a list of words from STRING that fail a spelling check."
  (let ((raw (with-input-from-string (input string)
               (run-program-to-string "aspell" '("list") :input input))))
    (when (plusp (length raw))
      (split-string-into-lines raw))))

(defun spellcheck-docstrings (package)
  "Print a list of all the functions in PACKAGE with document string spelllling errors."
  (with-package-iterator (iterator package :external :internal)
    (loop
      (multiple-value-bind (exists symbol) (iterator)
        (unless exists (return))
        (let* ((doc (documentation symbol 'function))
               (check (and doc (spellcheck doc))))
          (when check (format t "~S : ~{~A~^ ~}~%~%" symbol check)))))))
