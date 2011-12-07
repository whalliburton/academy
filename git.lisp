(in-package :academy)

(defun git (command &key (repository (asdf-system-base-path :academy)))
  (run-program-to-string
   "git" (cons (format nil "--git-dir=~A.git" repository) command)))

(defun git-list-commits (&key (repository (asdf-system-base-path :academy)))
  (split-string-into-lines
   (git '("log" "--reverse" "--format=format:%H") :repository repository)))

(defun git-commit-subject (commit &key (repository (asdf-system-base-path :academy)))
  (git `("log" "--format=format:%s" "-1" ,commit) :repository repository))

(defun git-list-branches (&key (repository (asdf-system-base-path :academy)))
  (mapcar (lambda (line) (string-trim '(#\space #\*) line))
          (split-string-into-lines
           (git `("branch") :repository repository))))

(defun git-branch (name &key (repository (asdf-system-base-path :academy))
                             commit)
  (git `("branch" ,name ,@(when commit `(,commit))) :repository repository))

(defun git-checkout (commit)
  (unless (member commit (git-list-branches) :test #'string=)
    (git-branch commit :commit commit))
  (git `("checkout" ,commit)))

