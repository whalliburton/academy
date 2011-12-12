(in-package :academy)

(defun ensure-list (element)
  (if (listp element)
    element
    (list element)))

(defun group (list n)
  "Group the elements of LIST into lists of N elements each."
  (when (zerop n) (error "Groups of zero are no fun."))
  (labels ((rec (list acc)
             (let ((rest (nthcdr n list)))
               (if (consp rest)
                 (rec rest (cons
                            (subseq list 0 n)
                            acc))
                 (nreverse
                  (cons list acc))))))
    (when list (rec list nil))))

(defun slurp-file (filename)
  "Load then entire file of FILENAME into a string."
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

(defun flatten (tree)
  "Return a list of all non-nil leaves of TREE."
  (let (result)
    (labels ((recur (subtree)
               (cond
                 ((consp subtree)
                  (recur (car subtree))
                  (recur (cdr subtree)))
                 ((not (null subtree))
                  (push subtree result)))))
      (recur tree)
      (nreverse result))))
