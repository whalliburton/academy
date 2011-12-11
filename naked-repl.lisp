(in-package :academy)

;;; USE-NAKED-REPL will turn a SLIME REPL into a REPL able to execute function calls
;;; without the outer set of parenthesis. This has the effect of turning the REPL into a
;;; command line.

(let ((original-eval-region))
  (defun naked-eval-region (string)
    (if (and (plusp (length string)) (char= (aref string 0) #\())
      (funcall original-eval-region string)
      (if (let* ((*read-eval* nil)
                 (first-element (read-from-string string)))
            (and (symbolp first-element) (fboundp first-element)))
        (funcall original-eval-region (format nil "(~A)" string))
        (funcall original-eval-region string))))
  (defun use-naked-repl (&optional (enable t))
    (if enable
      (if original-eval-region
        (format t "You are already using a naked repl.~%")
        (progn
          (setf original-eval-region (symbol-function (find-symbol "EVAL-REGION" :swank))
                (symbol-function (find-symbol "EVAL-REGION" :swank)) #'naked-eval-region)
          (format t "You are now using a naked repl.~%")))
      (progn
        (when original-eval-region
          (setf (symbol-function (find-symbol "EVAL-REGION" :swank)) original-eval-region
                original-eval-region nil))
        (format t "You are no longer using a naked repl.~%")))
    (values)))
