(in-package :academy)

(defmacro bugout (&rest vars)
  "Print VARS, for debugging."
  `(format t ,(with-output-to-string (s)
                (write-string "~%>>>" s)
                (loop for var in vars
                      do (write-string "  " s)
                         (prin1 var s)
                         (unless (keywordp var) (write-string " ~S" s)))
                (write-string "~2%" s))
           ,@(remove-if #'keywordp vars)))
