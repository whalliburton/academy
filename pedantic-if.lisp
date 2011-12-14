(in-package :academy)

(defmacro if (test then &optional (else nil else-defined))
  (unless else-defined (warn "A single legged IF should be replaced with a WHEN."))
  (when (and else-defined (null else))
    (warn "An if's else of NIL should be replaced with a WHEN."))
  `(cl:if ,test ,then ,else))
