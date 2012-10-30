(in-package :academy)

(defmacro %create-demo-images (&rest calls)
  `(progn
     ,@(loop for call in calls
             collect `(let ((*save-drawing-name*
                              ,(format nil "~{~(~A~)~^-~}" (ensure-list call))))
                        ,(ensure-list call)))))

(defun life-comic (&key (width 32) (height 32) (columns 11) (pattern "dh121") (scale 1))
  (let ((*bitmap* (with-comic-strip (:action identity :columns columns :width width :height height)
                    (life :steps (* columns columns) :size width :pattern pattern)))
        (*image-save-scale* scale))
    (draw *bitmap*)))

(defun create-demo-images ()
  (let ((*image-save-directory* "demo-images")
        (*image-save-directory-overwrite* t)
        (*image-save-inverse* t))
   (%create-demo-images
    ;; bullseye
    (bullseye :size 320)
    (bullseye :size 320 :filled t)
    ;; moiré
    (moiré :size 320 :offset 32)
    ;; sunbeam
    (sunbeam :size 320)
    (sunbeam :size 320 :step 3)
    ;; life
    (life-comic :width 32 :height 32 :columns 11 :pattern "dh121"))))
