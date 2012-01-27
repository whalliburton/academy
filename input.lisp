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

(defun read-number-from-keyboard (&key (message "Please enter a number.~%")
                                       allow-enter)
  (read-from-keyboard :validate (lambda (line)
                                  (if (and allow-enter (zerop (length line)))
                                    :enter
                                    (handler-case
                                        (parse-integer line)
                                      (parse-error ()))))
                      :message message))

(defun read-choice (choices &key (prompt "Enter your choice: ") default)
  (multiple-value-bind
        (choices returns)
      (loop with default-index
            for (name val) in choices
            for index from 1
            collect (if default
                      (list (if (equal default val) "*" "") index name)
                      (list index name))
            into choices
            collect val into returns
            finally (return (values choices returns default-index)))
    (print-table choices)
    (loop
      with num-choices = (length choices)
      for choice = (read-number-from-keyboard
                    :message (format nil "~A~@[~A~]" prompt
                                     (when default " [or press enter for the default] "))
                    :allow-enter (not (null default)))
      if (and (numberp choice) (or (> choice num-choices) (< choice 1)))
      do (warn "Selection out of range. Try again.")
      else do (return (if (eq choice :enter) default (nth (1- choice) returns))))))
