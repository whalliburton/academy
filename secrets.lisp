(in-package :academy)

(defun caesar-cypher (&optional (string (read-from-keyboard :message "Enter your message.~%"))
                                (offset (random 26)))
  "Encrypt using the classic Caesar's cipher, a substitution cipher."
  (values
   (coerce
    (loop for character across string
          collect (let ((index (- (char-code (char-upcase character)) (char-code #\A))))
                    ;;; !!! Note that we are only encrypting the alphabet letters.
                    (if (or (< index 0) (> index 26))
                      character
                      (code-char (+ (char-code #\a) (mod (+ index offset) 26))))))
    'string)
   (- 26 offset)))
