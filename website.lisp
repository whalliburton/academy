(in-package :academy)

(defun save-website ()
  (with-output-to-file ((format nil "~A/index.html" (asdf-system-base-path :academy)))
    (format
     t
     "<html>
      <head>
      <title>⚡ ⚕ The academy. ⚛ ♬</title>
      <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
      </head>
      <body style=\"background:black;color:white;font-size:xx-large;\"><pre>")
    (print-log)
    (princ "</pre></body></html>")))

