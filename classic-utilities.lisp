(in-package :academy)

(defun ensure-list (element)
  (if (listp element)
    element
    (list element)))
