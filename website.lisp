(in-package :academy)

(defun save-website (&key (font-size "24px"))
  (let ((index-filename (format nil "~Awebsite/index.html" (asdf-system-base-path :academy))))
    (ensure-directories-exist index-filename)
    (with-output-to-file (index-filename)
      (format t
       "<html>
<head>
<style type=\"text/css\">
@font-face{font-family:\"Terminus\"; src:url(\"terminus.ttf\") format(\"truetype\");}
pre {font-family:\"Terminus\", serif; font-size:~A; line-height:~A;}
</style>
<title>⚡ ⚕ The academy ⚛ ♬</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
</head>
<body style=\"background:black;color:white;font-size:24px;\"><pre>" font-size font-size)
      (print-log)
      (princ "</pre></body></html>"))
    (format t "The Academy website was saved to ~S." index-filename)))
