(in-package :academy)

;; For EVAL-IN-EMACS to work you need to enable in emacs, generally in the ~/.emacs file:
;; (setq slime-enable-evaluate-in-emacs t)

(defun eval-in-emacs (form)
  "Evalute FORM in the slime connected emacs and return its value. Note that many types
returned by emacs functions (such as buffers) are not serializable over the slime
connection and will result in an error."
  (funcall (find-symbol "EVAL-IN-EMACS" :swank) form))
