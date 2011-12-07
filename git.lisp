(in-package :academy)

(defun git (command &key (repository (asdf-system-base-path :academy)))
  (run-program-to-string
   "git" (cons (format nil "--git-dir=~A.git" repository) command)))
