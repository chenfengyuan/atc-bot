(declaim (optimize (debug 3)))

(defpackage :cfy.atc-bot
  (:use :cl :cl-ppcre))
(in-package :cfy.atc-bot)
(defvar *game* nil)
(defparameter *max-time* 100)
(defparameter *dir*
  #(
    (0 -1)				;0
    (1 -1)				;1
    (1 0)				;2
    (1 1)				;3
    (0 1)				;4
    (-1 1)				;5
    (-1 0)				;6
    (-1 -1)				;7
    (0 0)
    ))
(defvar *map* nil)
(defvar *map2* nil)

(defun good-dir-p (dir new_dir)
  (if (or
       (and (= dir 7) (= new_dir 0))
       (and (= dir 0) (= new_dir 7))
       (< (abs (- dir new_dir)) 2))
      t
      nil))
(defun good-pos-p (x y)
  (and (< -1 x (array-dimension *map* 0))
       (< -1 y (array-dimension *map* 1))
       (aref *map* x y)))
(defun best-a(dst_t a)
  (ecase dst_t
    (exit
     (if (< a 9)
	 '(1 0)
	 '(0)))
    (airport
     (cond 
       ((> a 1)
	'(-1 0))
       ((= a 1) '(0))
       ((= a 0) '(1 0))))))

(defun best-dir(d-x d-y x y)
  (cond
      ((and (< d-x x) (< d-y y)) '(7 6 0 1 5 2 4 3))
      ((and (= d-x x) (< d-y y)) '(0 7 1 2 6 3 5 4))
      ((and (> d-x x) (< d-y y)) '(1 0 2 7 3 4 6 5))
      ((and (< d-x x) (= d-y y)) '(6 5 7 0 4 1 3 2))
      ((and (> d-x x) (= d-y y)) '(2 1 3 0 4 5 7 6))
      ((and (< d-x x) (> d-y y)) '(5 4 6 3 7 0 2 1))
      ((and (= d-x x) (> d-y y)) '(4 3 5 2 6 1 7 0))
      ((and (> d-x x) (> d-y y)) '(3 2 4 1 5 0 6 7))))

(defun best-dir-dst(dst_n dst_t x y)
  (let* ((d_pos (nth dst_n
		     (nth
		      (ecase dst_t
			(exit 2)
			(airport 3))
		      *game*)))
	 (d-x (car d_pos))
	 (d-y (cadr d_pos))
	 d-dir)
    (ecase dst_t
      (exit
       (best-dir d-x d-y x y))
      (airport
       (setf d-dir (dir->num (caddr d_pos)))
       (cond
	 ((and (= d-dir 0) (<= y d-y))
	  (if (<= x d-x)
	      '(4 5 3 6 2 7 1 0)
	      '(4 3 5 2 6 1 7 0)))
	 ((and (= d-dir 2) (<= d-x x))
	  (if (<= y d-y)
	      '(6 7 5 0 4 1 3 2)
	      '(6 5 7 4 0 3 1 2)))
	 ((and (= d-dir 4) (<= d-y y))
	  (if (<= x d-x)
	      '(0 7 1 6 2 5 3 4)
	      '(0 1 7 2 6 3 5 4)))
	 ((and (= d-dir 6) (<= x d-x))
	  (if (<= y d-y)
	      '(2 1 3 0 4 7 5 6)
	      '(2 3 1 4 0 5 7 6)))
	 (t (best-dir d-x d-y x y)))))))
    
(defun safe-pos-p (x y a d time)
  (if (and
       (good-pos-p x y)
       (not (null (aref *map2* x y time)))
       (loop for deta-t in '(-1 0 1)
	  always (if (< -1 (+ deta-t time) *max-time*)
		     (loop for deta-a in '(-1 0 1)
			always (loop
				  for j from 0
				  for i across *dir*
				  for deta-x = (car i)
				  for deta-y = (cadr i)
				  always (if
					  (and (good-pos-p (+ x deta-x) (+ y deta-y)) (/= j (mod (+ d 4) 8)))
					  (= 0 (logand (ash 1 (+ a deta-a)) (aref *map2* (+ x deta-x) (+ y deta-y) time)))
					  t)))
		     t)))
      t))
(defun map2-set (x y a time)
  (setf (aref *map2* x y time) (logior (aref *map2* x y time) (ash 1 a))))
(defun map2-clr (x y a time)
  (setf (aref *map2* x y time) (logand (aref *map2* x y time) (lognot (ash 1 a)))))
(defun dir->num (d)
  (ecase d
    (w 0)
    (e 1)
    (d 2)
    (c 3)
    (x 4)
    (z 5)
    (a 6)
    (q 7)))
(defun make-map (&optional game)
  (let* ((game (if game
		   game
		   *game*))
	 (width (car game))
	 (height (cadr game))
	 (exits (caddr game))
	 (airports (cadddr game))
	 (map (make-array (list width height)))
	 (map2 (make-array (list width height *max-time*))))
    (loop for x from 0 to (1- width)
       do (setf (aref map x 0) nil
		(aref map x (1- height)) nil))
    (loop for y from 0 to (1- height)
       do (setf (aref map 0 y) nil
		(aref map (1- width) y) nil))
    (loop for x from 0 to (1- width)
       do (loop for time from 0 to (1- *max-time*)
	       do (setf (aref map2 x 0 time) nil
			(aref map2 x (1- height) time) nil)))
    (loop for y from 0 to (1- height)
       do (loop for time from 0 to (1- *max-time*)
	       do (setf (aref map2 0 y time) nil
			(aref map2 (1- width) y time) nil)))
    (loop
       for i in exits
       for n from 0
       for x = (car i)
       for y = (cadr i)
       do (setf (aref map x y) (list 'exit n))
       do (loop for time from 0 to (1- *max-time*)
	       do (setf (aref map2 x y time) 0)))
    (loop
       for i in airports
       for n from 0
       for x = (car i)
       for y = (cadr i)
       for d = (caddr i)
       do (setf (aref map x y) (list 'airport n (dir->num d)))
       do (loop for time from 0 to (1- *max-time*)
	     do (setf (aref map2 x y time) 0)))
    (list map map2)))

(defmacro decode-plane (sexp)
  `(let* ((i ,sexp)
	  (plane (car i))
	  (info (cdr i)))
     (list plane info)))

(defun get-planes (filename)
  (with-open-file (in filename)
    (loop for i = (read-line in nil nil)
       while i
       collect
	 (decode-plane (read-from-string i)))))

(defun dfs (x y a path dst_n dst_t fuel old-dir time)
  (if (< fuel 0)
      nil
      (let ((point (aref *map* x y))
	    (path (append path (list (list x y a)))))
  	(if (and (listp point)
  		 (eq (car point) dst_t)
  		 (eq (cadr point) dst_n)
  		 (or (and (eq dst_t 'exit)
			  (= a 9))
		     (and (eq dst_t 'airport)
			  (= a 1)
			  (= (caddr point) old-dir)
			  (= (caddr (car (last path 2))) 1)
			  (setf (caddr (car (last path))) 0))))
	    path
	    (if (= fuel 0)
		nil
		(loop
		   for deta-a in (best-a dst_t a)
		   for p = (loop
			      with p = nil
			      for i in (best-dir-dst dst_n dst_t x y)
			      for d = (elt *dir* i)
			      for deta-x = (car d)
			      for deta-y = (cadr d)
			      if (and (<= 0 (+ deta-a a) 9) (good-dir-p old-dir i) (safe-pos-p (+ x deta-x) (+ y deta-y) (+ a deta-a) i time))
			      do (setf p (dfs (+ x deta-x) (+ y deta-y) (+ a deta-a) path dst_n dst_t (1- fuel) i (1+ time)) )
			      until p
			      finally (return p))
		   until p
		   finally (return p)))))))

(defun mark-path (path plane_type)
  (loop
     for p in path
     for time from 0
     for x = (car p)
     for y = (cadr p)
     for a = (caddr p)
     do (map2-set x y a (if (= plane_type 0) time (truncate time 2))))
  path)
(defun make-map! (&optional game)
  (let ((maps (make-map game)))
    (setf *map* (car maps)
	*map2* (cadr maps)))
  t)
(defun search-dfs (plane_type x y a dst_n dst_t fuel d)
  (mark-path (dfs x y a nil dst_n dst_t fuel d 0) plane_type))

(defun action (path)
  (let* ((o (car path))
	 (n (cadr path))
	 (o-x (car o))
	 (o-y (cadr o))
	 (x (car n))
	 (y (cadr n))
	 (a (caddr n)))
    (list
     (cond
       ((and (= o-x x) (< y o-y)) "w")
       ((and (> x o-x) (< y o-y)) "e")
       ((and (> x o-x) (= y o-y)) "d")
       ((and (> x o-x) (> y o-y)) "c")
       ((and (= x o-x) (> y o-y)) "x")
       ((and (< x o-x) (> y o-y)) "z")
       ((and (< x o-x) (= y o-y)) "a")
       ((and (< x o-x) (< y o-y)) "q"))
     a)))
(defun get-next-actions (in-file)
  (make-map!)
  (loop
     for i in (get-planes in-file)
     for plane = (code-char (+ (char-code #\a) (car i)))
     for info = (cadr i)
     for action = (action (apply #'search-dfs info))
     collect (format nil "~aa~a~%~at~a~%" plane (cadr action) plane (car action))))

(defun main (in-file out-file)
  (with-open-file (out out-file :direction :output :if-exists :append :if-does-not-exist :create)
    (loop for i = (get-next-actions in-file)
       do (loop for j in i
	     do (princ j out)
	     do (finish-output out)
	     ;; do (princ j)
	       )
       do (sleep 3))))
