(in-package :cl-user)
(declaim (optimize (debug 3)))

(defpackage :cfy.atc-bot.input
  (:use :cl :cl-ppcre)
  (:export :get-map))
(in-package :cfy.atc-bot.input)

(defun map-line-filter (string)
  (car (last (all-matches-as-strings "[-0-9|+ .*^><v]{59,}" string))))

(defun get-map (atc &optional file-position-reset)
  (if file-position-reset
      (file-position atc 0))
  (loop
     for i = (read-line atc nil nil)
     do (format t "~a~%" (map-line-filter i))
     until (char= #\- (elt i 0))))
