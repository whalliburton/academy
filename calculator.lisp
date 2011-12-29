(in-package :academy)

;;; Our own Reverse Polish Notation Calculator based on the UserRPL language of the
;;; impressive line of HP calculators!

;;; A stack based language in the lineage of FORTH.

;;; The state of the calculator is kept in the following special variables.  Since the
;;; variables are "special", we get a new set of them each time we instantiate the
;;; calculator. By not giving them values (i.e. leaving them "unbound"), we are also
;;; ensuring that functions either use them in their proper context or error out.

(defvar stack)       ; !!! We omit earmuffs (**) here for readabilty.
(defvar undo-stack)
(defvar base)
(defvar angle-mode)
(defvar random-generator-state)
(defvar variables)
(defvar program)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun calc-fun (name)
    (intern (mkstr '$-calculator-command- name) :academy)))

;;; Out first mega macro shotgunning DEFUNs!!!

;;; The entire point of the DEFINE-CALCULATOR-COMMANDS macro is to create a simple domain
;;; specific language (DSL) for the defining of calculator commands.

;;; Macros like these are created iteratively and not just written outright, but once
;;; grown this way they are often times best understood first through their expansion and
;;; secondarily though reading and understanding the macro code.

;;; Examples:
;;;
;;;  (macroexpand '(define-calculator-commands sin))
;;;  (macroexpand '(define-calculator-commands (foo 2 (push (+ 42 (* first second))))))

;;; Attention Hackers!! Try those commands above ↑↑↑↑ on your own REPL.

(define-condition skip-ahead () ((to :initarg :to :reader skip-ahead-to)))

(defmacro define-calculator-commands (&rest definitions)
  `(flet ((push (obj) (push obj stack))
          (skip (&rest to) (signal 'skip-ahead :to to))
          (torad (val) (ecase angle-mode
                         (:degrees (* val (/ pi 180)))
                         (:radians val)))
          (fromrad (val)
            (ecase angle-mode
              (:degrees (* val (/ 180 pi)))
              (:radians val))))
     (macrolet ((logic (test) `(if ,test 1 0)))
       ,@(loop for definition in definitions
               unless (stringp definition) ;; strings signify group titles
               collect
                  (destructuring-bind (name arguments . body)
                      (if (consp definition) definition `(,definition -1))
                    (destructuring-bind (&optional num-arguments)
                        (ensure-list arguments)
                      `(defun ,(calc-fun name) ()
                         ,@(when arguments
                             `((when (null (nth ,(1- (abs num-arguments)) stack))
                                 (error "Too Few Arguments"))))
                         (destructuring-bind (&optional first second third &rest rest) stack
                           (declare (ignorable first second third rest))
                           ,@(when (and arguments (minusp num-arguments))
                               `((setf stack (nthcdr ,(abs num-arguments) stack))))
                           ,@(if body
                               body
                               `((push (,name first))))))))))
     ,@(flet ((definition-name (definition)
                (destructuring-bind (name . body) (ensure-list definition)
                  (declare (ignore body))
                  name)))
         `((defparameter *calculator-commands*
             ',(loop for definition in definitions
                     unless (stringp definition)
                     collect (definition-name definition)))
           (defparameter *calculator-help*
             ',(loop for definition in definitions
                     if (stringp definition)
                     collect definition
                     else collect (definition-name definition)))))))

(define-calculator-commands

  "Where to start"
  (help nil (show-calculator-help))
  (settings nil (show-calculator-settings))

  "How to exit"
  (quit nil (throw 'quit nil))

  "Stack Handling"
  (clear  nil  (setf stack nil))
  (prst   nil  (print-stack))
  (depth  nil  (push (length stack)))
  (drop   -1   nil)
  (drop2  -2   nil)
  (dropn  2    (setf stack (nthcdr (1+ first) stack)))
  (dup    1     (push first))
  (dup2   2    (push first) (push first))
  (dupn   2    (setf stack
                     (append
                      (loop for row in (cdr stack)
                            for index from 1 to first
                            collect row)
                       (cdr stack))))
  (over   2    (setf stack (cons second stack)))
  (pick   2    (setf stack (cons (nth (1+ first) stack) (cdr stack))))
  (roll   2    (loop for index from 1 to first
                     for list on stack
                     finally (let ((el (second list)))
                                (setf (cdr list) (cddr list))
                               (setf stack (cons el (cdr stack))))))
  (rolld  2    (loop for index from 1 to first
                     for list on (cdr stack)
                     finally (cl:push second (cdr list))
                             (setf stack (cddr stack))))
  (rot    3    (rotatef (first stack) (third stack)))
  (swap   2    (rotatef (first stack) (second stack)))
  (undo nil    (setf stack undo-stack))

  "Arithmetical Operations"
  (+ -2 (push (+ first second)))            ; Plus
  (- -2 (push (- second first)))            ; Minus
  (* -2 (push (* first second)))            ; Multiply
  (/ -2 (push (/ second first)))            ; Divide
  (^ -2 (push (expt first second)))         ; Power
  (= -2 (push (logic (= first second))))    ; Equals
  abs                                       ; Absolute Value
  exp                                       ; Exponential
  (inv -1 (push (/ 1 first)))               ; Inverse
  sqrt                                      ; Square Root
  (arg -1                                   ; Argument
       (push
        (if (zerop (realpart first))
          (/ pi 2)
          (+ (atan (/ (imagpart first)
                      (realpart first)))
             (if (< (realpart first) 0)
               (ecase angle-mode
                 (:radians pi)
                 (:degrees 180))
               0)))))
  (asr  -1 (push (ash first -1)))           ; Arithmetic Shift Right
  (ceil -1 (push (ceiling first)))          ; Ceiling
  (rnd  -1 (push (round first)))            ; Round
  (%ch  -2                                  ; Percent Change
        (push (* 100 (/ (- first second)
                        second))))
  (conj -1                                  ; Conjugate
        (push (conjugate first)))
  (c->r -1                                  ; Complex to Real
        (push (realpart first))
        (push (imagpart first)))
  (r->c -2                                  ; Real to Complex
        (push (complex second first)))
  (im -1 (push (imagpart first)))           ; Imaginary Part
  (re -1 (push (realpart first)))           ; Real Part
  (decr nil (decf (first stack)))           ; Decrement
  (incr nil (incf (first stack)))           ; Increment
  (! -1 (push (factorial first)))           ; Factorial
  (fp -1                                    ; Fractional Part
      (multiple-value-bind (whole part)
          (truncate first)
        (declare (ignore whole))
        (push part)))
  (max -2 (push (max first second)))        ; Maximum
  (min -2 (push (min first second)))        ; Minimum
  (mod -2 (push (mod second first)))        ; Modulo
  (neg -1 (push (- first)))                 ; Negate
  (sign -1                                  ; Sign
        (push (cond
                ((plusp first) 1)
                ((zerop first) 0)
                (t -1))))
  (sq -1 (push (* first first)))            ; Square
  (%t -2                                    ; Percent of Total
      (push (/ (* 100 first) second)))
  (floor -1 (push (floor first)))           ; Floor
  (fp -1 (push (second                      ; Fractional Part
                (multiple-value-list
                 (truncate first)))))
  (ip -1 (push (truncate first)))           ; Integer Part

  "Trigonometry"
  (cos -1 (push (cos (torad first))))       ; Cosine
  (sin -1 (push (sin (torad first))))       ; Sine
  (tan -1 (push (tan (torad first))))       ; Tangent
  (acos -1 (push (fromrad (acos first))))   ; Arc Cosine
  (asin -1 (push (fromrad (asin first))))   ; Arc Sine
  (atan -1 (push (fromrad (atan first))))   ; Arc Tangent
  (r->d -1 (push (* (/ 180 pi) first)))     ; Radians to Degrees
  (d->r -1 (push (* (/ pi 180) first)))     ; Degrees to Radians
  (deg nil (setf angle-mode :degrees))      ; Degrees
  (rad nil (setf angle-mode :radians))      ; Radians

  "Logarithms"
  acosh                                     ; Inverse Hyperbolic Cosine
  asinh                                     ; Arc Hyperbolic Sine
  atanh                                     ; Arc Hyperbolic Tangent
  cosh                                      ; Hyperbolic Cosine
  sinh                                      ; Hyperbolic Sine
  tanh                                      ; Hyperbolic Tangent
  (alog -1 (push (expt 10 first)))          ; Common Antilogarithm
  (ln -1 (push (log first)))                ; Natural Logarithm
  (log -1 (push (log first 10)))            ; Common Logarithm

  "Constants"
  (pi nil (push (symbol-value 'pi)))        ; PI
  (e nil (push (exp 1.0d0)))                ; e
  (i nil (push #c(0 1)))                    ; i

  "Logical and Binary Operations"
  (bin nil (setf base 2))                   ; Binary Mode
  (oct nil (setf base 8))                   ; Octal Mode
  (dec nil (setf base 10))                  ; Decimal Mode
  (hex nil (setf base 16))                  ; Hexidecimal Mode
  (sl -1 (push (ash first 1)))              ; Shift Left
  (slb -1 (push (ash first 8)))             ; Shift Left Byte
  (sr -1 (push (ash first -1)))             ; Shift Right
  (srb -1 (push (ash first -8)))            ; Shift Right Byte
  (< -2 (push (logic (< second first))))    ; Less Than
  (> -2 (push (logic (> second first))))    ; Greater Than
  (<= -2 (push (logic (<= second first))))  ; Less Than or Equal
  (>= -2 (push (logic (>= second first))))  ; Greater Than or Equal
  (and -2                                   ; And
       (push (logic
              (and (not (zerop first))
                   (not (zerop second))))))
  (or -2                                    ; Or
       (push (logic
              (or (not (zerop first))
                  (not (zerop second))))))
  (not -1 (push (logic (zerop first))))     ; Not

  "Input/Output"
  (disp -1 (format t "~A~%" first))         ; Display
  (input -1 (format t "~A" first)           ; Input
         (force-output)
         (push (safe-read)))

  "Statistics"
  (perm -2                                  ; Permutations
        (push
         (if (< second first)
           0
           (/ (factorial second)
              (factorial (- second first))))))
  (rand nil (push (random 1.0d0)))          ; Random Number
  (rdz -1 #+sbcl (setf *random-state*       ; Randomize
                       (if (zerop first)
                         (make-random-state t)
                         (sb-ext:seed-random-state first)))
          #-sbcl (error "RDZ is not available."))

  "Control Structures"
  (if nil nil)                              ; If
  (then -1                                  ; Then
        (unless (not (zerop first))
          (skip 'end 'else)))
  (else nil (skip 'end))                    ; Else
  (end nil nil)                             ; End

  "Variables"
  (sto -2                                   ; Store
       (assert (symbolp first))
       (setf (gethash first variables) second))

  "Programs"
  (<< nil (setf program (list '<<)))        ; Start program
  (>> nil nil)                              ; End program

  )

;;; Here is where we bind new values to our special variables for each calculator
;;; instantiation.

(defmacro with-fresh-calculator (&body body)
  `(let ((stack nil)
         (undo-stack nil)
         (base 10)
         (angle-mode :radians)
         (variables (make-hash-table))
         (program nil)
         (*random-state* (make-random-state t)))
     ,@body))

(defun calculator (&key (intro t))
  "RPN calculator. Try 'help'. Exit with 'quit'."
  (when intro (format t "Welcome to the academy's reverse polish notation (RPN) calculator.~%"))
  (catch 'quit
    (with-fresh-calculator
      (loop
        do (print-stack)
           (format t "> ")
           (force-output)
           (let ((stack-copy (copy-list stack)))
             (%calculate
              (with-input-from-string (stream (read-line))
                (loop
                  for command = (safe-read stream nil)
                  while command
                  collect command)))
             (setf undo-stack stack-copy))))))

(defun %calculate (list)
  (loop
    with remaining = list
    do (let ((command (car remaining)))
         (cond
           (program
            (if (eq command '>>)
              (progn
                (push (coerce (cdr (nreverse program)) 'vector) stack)
                (setf program nil))
              (push command program)))
           ((null command) (return))
           ((or (numberp command) (stringp command)) (push command stack))
           ((member command *calculator-commands* :test #'string-equal)
            (handler-case
                (funcall (calc-fun command))
              (error (condition)
                (format t "Error: ~A~%" condition))
              (skip-ahead (condition)
                (loop with to = (skip-ahead-to condition)
                      for el on remaining
                      when (member (car el) to)
                      do (setf remaining el) (return)
                      finally (format t "Missing ~{~S~^ ~}.~%" to)))))
           ((symbolp command)
            (let ((value (gethash command variables)))
              (if (vectorp value)
                (setf (cdr remaining) (nconc (coerce value 'list) (cdr remaining)))
                (push (or value command) stack))))
           (t (format t "Invalid input.~%")))
         (setf remaining (cdr remaining)))
    finally (return (first stack))))

(defmacro calculate (&rest commands)
  "Inline REPL RPN calculator."
  `(with-fresh-calculator
     (%calculate ',commands)
     (first stack)))

(defun print-stack ()
  (loop for index from 1
        for row in (reverse stack)
        do (typecase row
             (float (format t "~F" row))
             (number (format t (format nil "~~~AR" base) row))
             (symbol (format t "'~A'" row))
             (vector (format t "<< ~{~A~^ ~} >>" (coerce row 'list)))
             (t (format t "~S" row)))
           (fresh-line)))

(defun show-calculator-help ()
  (loop with heading and acc
        for el in *calculator-help*
        if (stringp el)
        do #1=(when acc
                (format t "~A~%" heading)
                (print-table (mapcar (lambda (row) (cons "" row))
                                     (group (sort acc #'string<) 8))))
           (setf heading el acc nil)
        else do (push el acc)
        finally #1#))

(defun show-calculator-settings ()
  (format t "base: ~A  angles: ~(~A~)~%" base angle-mode))

;;; Examples

;; << swap over / ceil * >> up sto
;; << over swap mod - >> down sto
;; << if dup 2 mod then 3 * 1 + else 2 / end >> ulam sto
