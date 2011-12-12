(in-package :academy)

(flet ((generate-control-string (vars)
         (with-output-to-string (s)
           (loop for var on vars
                 do (prin1 (car var) s)
                    (unless (keywordp (car var)) (write-string " ~S" s))
                    (when (cdr var) (format s "  "))))))

  (defmacro bugout (&rest vars)
    "Print VARS, for debugging."
    `(format t ,(with-output-to-string (s)
                  (write-string "~%>>>  " s)
                  (write-string (generate-control-string vars) s)
                  (write-string "~2%" s))
             ,@(remove-if #'keywordp vars)))

  (defmacro breakout (&rest vars)
    "Break with VARS, for debugging."
    `(break ,(generate-control-string vars)
            ,@(remove-if #'keywordp vars))))
