(in-package :academy)

;;; SBCL specific utilities.

;;; Attention non-SBCL hackers! Compatibility code for your lisp of choice would be
;;; happily integrated here! For the betterment of lisp and the furtherment of humanity's
;;; hopefully everlasting search for objective truths.

(defun ensure-process-success (process &key (expected-exit-code 0)
                                            (expected-status :exited))
  (let ((status (sb-ext:process-status process))
        (exit-code (sb-ext:process-exit-code process)))
    (unless (eql status expected-status)
      (error "process-failure-status"))
    (unless (eql exit-code expected-exit-code)
      (error "process-failure-exit"))
    process))

(defun run-program-to-string (program args &key (input t))
  "Run system program PROGRAM with arguments ARGS.
Returns all the output of standard output and standard error as a string."
  (with-output-to-string (stream)
    (ensure-process-success
     (sb-ext:run-program program args :output stream :input input :error stream :search t))))
