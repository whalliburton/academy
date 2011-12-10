(in-package :academy)

(defun read-from-keyboard (&key (prompt "⌨   ") validate message)
  (when message (format t message))
  (loop
    do (when prompt (format t "~A" prompt))
       (force-output)
       (let* ((line (read-line))
              (valid (or (and (null validate) line)
                         (funcall validate line))))
         (when valid (return valid)))
    (format t "Invalid entry. Please try again.~%")))

(defun read-number-from-keyboard (&key (message "Please enter a number.~%"))
  (read-from-keyboard :validate (lambda (line)
                                  (handler-case
                                      (parse-integer line)
                                    (parse-error ())))
                      :message message))
