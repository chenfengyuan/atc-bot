(declaim (optimize (debug 3) (speed 1) (compilation-speed 3)))

(defpackage :cfy.atc-bot
  (:use :cl :alexandria :inotify :trivial-timeout :ct :cl-heap)
  (:export :plane-pos :get-plane-nums :get-infos :calculate-paths :plane-type))
(in-package :cfy.atc-bot)
(defvar *game* nil)
(defparameter *max-time* 420)
(defvar *infos* nil)
(defvar *planes* nil)
(defvar *base-time* nil)
;; (defvar *actions* (make-hash-table))
(defvar *map* nil)
(defvar *ct* nil)
(defvar *far-distance* 7)
(defvar *max-step* 50)
(defvar *a*)
(defvar *b*)
(setf (symbol-function 's) (symbol-function 'shuffle))
(defun deta-xy ()
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
;; (defparameter *deta-xy*
;;   #(
;;     (0 -1)				;0
;;     (1 -1)				;1
;;     (1 0)				;2
;;     (1 1)				;3
;;     (0 1)				;4
;;     (-1 1)				;5
;;     (-1 0)				;6
;;     (-1 -1)				;7
;;     (0 0)
;;     ))

(defun get-dirs (actions old-dir)
  (loop
     for i in actions
     collect (mod (+ (ecase i
		       (l -1)
		       (hl -2)
		       (r 1)
		       (hr 2)
		       (s 0)) old-dir)
		  8)))
(defun dirs->deta-xys (dirs)
  (loop for i in dirs
     collect (aref (deta-xy) i)))

(defun forwardp (x0 y0 dir x1 y1)
  (declare (type fixnum x0 y0 dir x1 y1))
  (ecase dir
    (0 (> y0 y1))
    (2 (< x0 x1))
    (4 (< y0 y1))
    (6 (> x0 x1))))
(defun backwardp (x0 y0 dir x1 y1)
  (declare (type fixnum x0 y0 dir x1 y1))
  (ecase dir
    (0 (< y0 y1))
    (2 (> x0 x1))
    (4 (> y0 y1))
    (6 (< x0 x1))))
(defun parrelp (x0 y0 dir x1 y1)
  (declare (type fixnum x0 y0 dir x1 y1))
  (ecase dir
    (0 (= y0 y1))
    (2 (= x0 x1))
    (4 (= y0 y1))
    (6 (= x0 x1))))
(defun leftp (x0 y0 dir x1 y1)
  (declare (type fixnum x0 y0 dir x1 y1))
  (ecase dir
    (0 (> x0 x1))
    (2 (> y0 y1))
    (4 (< x0 x1))
    (6 (< y0 y1))))
(defun rightp (x0 y0 dir x1 y1)
  (declare (type fixnum x0 y0 dir x1 y1))
  (ecase dir
    (0 (< x0 x1))
    (2 (< y0 y1))
    (4 (> x0 x1))
    (6 (> y0 y1))))
(defun middlep (x0 y0 dir x1 y1)
  (declare (type fixnum x0 y0 dir x1 y1))
  (ecase dir
    (0 (= x0 x1))
    (2 (= y0 y1))
    (4 (= x0 x1))
    (6 (= y0 y1))))

(defun next-dirs (a x y dir dst-x dst-y dst-dir dst-type)
  (if (= a 0)
      `(,dir)
      (ecase dst-type
	(airport
	 (let ((tmp (next-dirs-airport x y dir dst-x dst-y dst-dir)))
	   (if (numberp (car tmp))
	       tmp
	       (get-dirs tmp dir))))
	(exit
	 (next-dirs-exit x y dst-x dst-y dst-dir)))))
(defun next-dirs-airport (x y dir dst-x dst-y dst-dir)
  (declare (type fixnum x y dir dst-x dst-y dst-dir))
  (if (and (not (backwardp dst-x dst-y dst-dir x y)) (member dir '(1 3 5 7)))
      (fix-dir)
      (if (backwardp dst-x dst-y dst-dir x y)
	  (let ((d-x dst-x) (d-y dst-y))
	    (declare (type fixnum d-y d-x))
	    (ecase dst-dir
	      (0 (incf d-y))
	      (2 (decf d-x))
	      (4 (decf d-y))
	      (6 (incf d-x)))
	    (if (and (= d-x x) (= d-y y))
		(list (position (list (- dst-x x)
				      (- dst-y y))
				(deta-xy) :test #'equalp))
		(cond
		  ((and (> d-x x) (> d-y y)) '(3 1 5 2 4 0 6 7))
		  ((and (> d-x x) (= d-y y)) '(2 0 4 1 3 5 7 6))
		  ((and (> d-x x) (< d-y y)) '(1 7 3 0 2 6 4 5))
		  ((and (= d-x x) (> d-y y)) '(4 5 3 6 2 7 1 0))
		  ((and (= d-x x) (< d-y y)) '(0 7 1 6 2 5 3 4))
		  ((and (< d-x x) (> d-y y)) '(5 6 4 7 3 0 2 1))
		  ((and (< d-x x) (= d-y y)) '(6 7 5 0 4 1 3 2))
		  ((and (< d-x x) (< d-y y)) '(7 0 6 1 5 2 4 3)))))
	  (let ((sf (forwardp x y dir dst-x dst-y))
		(sb (backwardp x y dir dst-x dst-y))
		(sl (leftp x y dir dst-x dst-y))
		(sr (rightp x y dir dst-x dst-y))
		(df (forwardp dst-x dst-y dst-dir x y))
		(db (backwardp dst-x dst-y dst-dir x y))
		(dp (parrelp dst-x dst-y dst-dir x y))
		(dl (leftp dst-x dst-y dst-dir x y))
		(dr (rightp dst-x dst-y dst-dir x y))
		(dm (middlep dst-x dst-y dst-dir x y)))
	    (cond
	      (df
	       (cond
		 (dl
		  (if sf
		      (if sr
			  '(hr hl s)
			  '(s hr hl))
		      '(hl s hr)))
		 (dr
		  (if sf
		      (if sl
			  '(hl hr s)
			  '(s hl hr))
		      '(hr s hl)))
		 (dm
		  (cond
		    (sf '(hl hr s))
		    (sb '(hl hr s))
		    (sr '(hr s hl))
		    (sl '(hl s hr))))))
	      (dp
	       (cond
		 (dl
		  (cond
		    (sf '(hr hl s))
		    (sb '(hl s hr))
		    (sr '(hl s hr))
		    (sl '(s hr hl))))
		 (dr
		  (cond
		    (sf '(hl hr s))
		    (sb '(hr s hl))
		    (sr '(s hr hl))
		    (sl '(s hl hr))))
		 (t
		  '(s))))
	      (db
	       (error "this should not happen db")
	       ;; (cond
	       ;; 	 (dl
	       ;; 	  (if sf
	       ;; 	      (if sl
	       ;; 		  '(s hl hr)
	       ;; 		  '(hr hl s))
	       ;; 	      '(hl s hr)))
	       ;; 	 (dr
	       ;; 	  (if sf
	       ;; 	      (if sr
	       ;; 		  '(s hr hl)
	       ;; 		  '(hl hr s))
	       ;; 	      '(hr s hl)))
	       ;; 	 (dm
	       ;; 	  (cond
	       ;; 	    (sf '(s hl hr))
	       ;; 	    (sb '(hl hr s))
	       ;; 	    (sr '(hr s hl))
	       ;; 	    (sl '(hl s hr)))))
	       ))))))
(defun next-dirs-exit (x y dst-x dst-y dst-dir)
  (declare (type fixnum x y dst-x dst-y dst-dir))
  (declare (ignore dst-dir))
  (cond
    ((and (> dst-x x) (> dst-y y)) `(3 ,@(s '(4 2)) ,@(s '(5 1)) ,@(s '(0 6)) 7))
    ((and (> dst-x x) (= dst-y y)) `(2 ,@(s '(3 1)) ,@(s '(4 0)) ,@(s '(5 7)) 6))
    ((and (> dst-x x) (< dst-y y)) `(1 ,@(s '(2 0)) ,@(s '(3 7)) ,@(s '(4 6)) 5))
    ((and (= dst-x x) (> dst-y y)) `(4 ,@(s '(5 3)) ,@(s '(6 2)) ,@(s '(7 1)) 0))
    ((and (= dst-x x) (< dst-y y)) `(0 ,@(s '(1 7)) ,@(s '(2 6)) ,@(s '(3 5)) 4))
    ((and (< dst-x x) (> dst-y y)) `(5 ,@(s '(6 4)) ,@(s '(7 3)) ,@(s '(0 2)) 1))
    ((and (< dst-x x) (= dst-y y)) `(6 ,@(s '(7 5)) ,@(s '(0 4)) ,@(s '(1 3)) 2))
    ((and (< dst-x x) (< dst-y y)) `(7 ,@(s '(0 6)) ,@(s '(1 5)) ,@(s '(2 4)) 3))))
(defun good-dir-p (old-dir new-dir)
  (declare (type fixnum old-dir new-dir))
  (if (or
       (<= (mod (- old-dir new-dir) 8)2)
       (<= (mod (- new-dir old-dir) 8) 2))
      t))
(defun good-pos-p (x y)
  (declare (type fixnum x y))
  (and (< 0 x (car *game*))
       (< 0 y (cadr *game*))
       (aref *map* x y 0)))
(defun next-a-exit (a distance)
  (declare (type fixnum a))
  (if (> distance *far-distance*)
      (cond
	((or (= a 7) (= a 8))
	 '(0 1 -1))
	((< 1 a 7)
	 '(1 0 -1))
	((= a 9)
	 '(0 -1))
	((= a 1)
	 '(1 0))
	(t
	 '(1)))
      (cond
	((< 1 a 9)
	 '(1 0 -1))
	((= a 9)
	 '(0 -1))
	((= a 1)
	 '(1 0))
	(t
	 '(1)))))
(defun next-a-airport (a distance)
  (declare (type fixnum a)
	   (ignore distance))
  (cond
    ((< 1 a 9)
     '(-1 0 1))
    ((= a 9)
     '(-1 0))
    ((= a 1)
     '(0 1))
    (t
     '(1))))

(defun next-a (a dst-type distance)
  (ecase dst-type
    (airport (next-a-airport a distance))
    (exit (next-a-exit a distance))))

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
(defun unmark-destination (&optional (game *game*))
  (let ((exits (caddr game))
	(airports (cadddr game))
	(max-time *max-time*))
    (declare (type fixnum max-time))
    (loop
       for i in exits
       for x = (car i)
       for y = (cadr i)
       do (loop for time from 0 to (1- max-time)
    	     do (map-clr x y 9 time)))
    (loop
       for i in airports
       for x = (car i)
       for y = (cadr i)
       do (loop for time from 0 to (1- max-time)
	     do (map-clr x y 0 time)))))
(defun make-map (&optional game)
  (let* ((game (if game
		   game
		   *game*))
	 (width (car game))
	 (height (cadr game))
	 (exits (caddr game))
	 (airports (cadddr game))
	 (map (make-array (list width height *max-time*))))
    (declare (type fixnum width height *max-time*))
    (loop for x from 0 to (1- width)
       do (loop for time from 0 to (1- *max-time*)
	     do (setf (aref map x 0 time) nil
		      (aref map x (1- height) time) nil)))
    (loop for y from 0 to (1- height)
       do (loop for time from 0 to (1- *max-time*)
	     do (setf (aref map 0 y time) nil
		      (aref map (1- width) y time) nil)))
    (loop
       for i in exits
       for x = (car i)
       for y = (cadr i)
       do (loop for time from 0 to (1- *max-time*)
    	     do (setf (aref map x y time) 0)))
    (loop
       for i in airports
       for x = (car i)
       for y = (cadr i)
       do (loop for time from 0 to (1- *max-time*)
	     do (setf (aref map x y time) 0)))
    (setf *map* map)
    t))

(defun step->times (step plane-type base-time)
  (ecase plane-type
    (0
     (if (oddp base-time)
	 (if (= step 0)
	     '(0)
	     `(,(1- (* 2 step))
		,(* 2 step)))
	 `(,(* 2 step)
	    ,(1+ (* 2 step)))))
    (1
     `(,step))))

(defun safe-pos-p (x y a step plane-type dst-x dst-y dst-type dir dst-dir base-time)
  (if (and (= x dst-x)
	   (= y dst-y)
	   (ecase dst-type
	     (exit (= a 9))
	     (airport (and (= a 0)
			   (eq dir dst-dir)))))
      t
      (if (and
	   (> a 0)
	   (if (> step 0)
	       (and (< 0 x (1- (car *game*)))
		    (< 0 y (1- (cadr *game*))))
	       t)
	   (good-pos-p x y)
	   (not (null (aref *map* x y step)))
	   (loop
	      ;; for time in (if (= plane-type 0) `(,(max 0 (1- (* 2 step))) ,(* 2 step) ,(+ 1 step step) ,(+ 2 step step)) `(,step))
	      ;; for time in (if (= plane-type 0)
	      ;; 		      `(,(+ step step) ,(+ 1 step step))
	      ;; 		      `(,step))
	      for time in (step->times step plane-type base-time)
	      always (loop for deta-a in '(-1 0 1)
			always (loop
				  for j from 0
				  for i across (deta-xy)
				  for deta-x = (car i)
				  for deta-y = (cadr i)
				  ;; do (format t "~a,~a,~a " deta-x deta-y time)
				  always (if
					  (good-pos-p (+ x deta-x) (+ y deta-y)) 
					  (= 0 (logand (ash 1 (+ a deta-a)) (aref *map* (+ x deta-x) (+ y deta-y) time)))
					  t)))))
	  t)))

;; (defun best-a(dst_t a)
;;   (ecase dst_t
;;     (exit
;;      (if (< a 9)
;; 	 '(1 0)
;; 	 '(0)))
;;     (airport
;;      (cond 
;;        ((> a 1)
;; 	'(-1 0 1))
;;        ((= a 1) '(0 1))
;;        ((= a 0) '(1))))))

;; (defun best-dir(d-x d-y x y)
;;   (cond
;;     ((and (< d-x x) (< d-y y)) '(7 6 0 1 5 2 4 3))
;;     ((and (= d-x x) (< d-y y)) '(0 7 1 2 6 3 5 4))
;;     ((and (> d-x x) (< d-y y)) '(1 0 2 7 3 4 6 5))
;;     ((and (< d-x x) (= d-y y)) '(6 5 7 0 4 1 3 2))
;;     ((and (> d-x x) (= d-y y)) '(2 1 3 0 4 5 7 6))
;;     ((and (< d-x x) (> d-y y)) '(5 4 6 3 7 0 2 1))
;;     ((and (= d-x x) (> d-y y)) '(4 3 5 2 6 1 7 0))
;;     ((and (> d-x x) (> d-y y)) '(3 2 4 1 5 0 6 7))))

;; (defun best-dir-dst(dst_n dst_t x y)
;;   (let* ((d_pos (nth dst_n
;; 		     (nth
;; 		      (ecase dst_t
;; 			(exit 2)
;; 			(airport 3))
;; 		      *game*)))
;; 	 (d-x (car d_pos))
;; 	 (d-y (cadr d_pos))
;; 	 d-dir)
;;     (ecase dst_t
;;       (exit
;;        (best-dir d-x d-y x y))
;;       (airport
;;        (setf d-dir (dir->num (caddr d_pos)))
;;        (cond
;; 	 ((and (= d-dir 0) (<= y d-y))
;; 	  '(4 3 5 2 6 1 7 0))
;; 	 ((and (= d-dir 2) (<= d-x x))
;; 	  '(6 7 5 0 4 1 3 2))
;; 	 ((and (= d-dir 4) (<= d-y y))
;; 	  '(0 1 7 2 6 3 5 4))
;; 	 ((and (= d-dir 6) (<= x d-x))
;; 	  '(2 3 1 4 0 5 7 6))
;; 	 ((and (< d-x x) (< d-y y)) '(4 2 1 5 0 6 7 3))
;; 	 ((and (= d-x x) (< d-y y)) '(4 2 1 0 7 6 5 3))
;; 	 ((and (> d-x x) (< d-y y)) '(4 6 3 7 2 0 1 5))
;; 	 ((and (< d-x x) (= d-y y)) '(2 0 4 5 7 6 1 3))
;; 	 ((and (> d-x x) (= d-y y)) '(6 0 4 1 3 2 5 7))
;; 	 ((and (< d-x x) (> d-y y)) '(0 2 3 7 4 6 5 1))
;; 	 ((and (= d-x x) (> d-y y)) '(0 2 6 3 5 4 1 7))
;; 	 ((and (> d-x x) (> d-y y)) '(0 6 1 5 2 4 3 7)))))))

(defun map-set (x y a time)
  (unless (eq a nil)
    (setf (aref *map* x y time) (logior (aref *map* x y time) (ash 1 a)))))
(defun map-clr (x y a time)
  (unless (eq a nil)
    (setf (aref *map* x y time) (logand (aref *map* x y time) (lognot (ash 1 a))))))

(defmacro decode-plane (sexp)
  `(let* ((i ,sexp)
	  (plane (car i))
	  (infos (cdr i)))
     (list plane infos)))

(defun get-infos (filename)
  (with-open-file (in filename)
    (cons (parse-integer (read-line in nil nil))
	  (loop for i = (read-line in nil nil)
	     while i
	     collect
	       (decode-plane (read-from-string i))))))
(defun get-infos! (filename)
  (setf *infos* (get-infos filename)))

(defun get-plane-nums (infos)
  (loop for i in (cdr infos)
     collect (car i)))
;; (defun distance (dst_n dst_t x y)
;;   (let* ((d_pos (nth dst_n
;; 		     (nth
;; 		      (ecase dst_t
;; 			(exit 2)
;; 			(airport 3))
;; 		      *game*)))
;; 	 (dx (car d_pos))
;; 	 (dy (cadr d_pos)))
;;     (+ (abs (- x dx))
;;        (abs (- y dy)))))

;; (defun plane-distance (info)
;;   (let ((dst_t (nth 5 info))
;; 	(dst_n (nth 4 info))
;; 	(x (cadr info))
;; 	(y (caddr info)))
;;     (distance dst_n dst_t x y)))
;; (defun get-planes-sorted (filename)
;;   (sort (get-planes filename)
;; 	(lambda (x y)
;; 	  (or
;; 	   (< (caadr x) (caadr y))
;; 	   (< (plane-distance (cadr x)) (plane-distance (cadr y)))
;; 	   (< (nth 6 (cadr x)) (nth 6 (cadr y)))))))
(defun fix-dir ()
  '(0 1 2 3 4 5 6 7))

(defun dfs (x y a path dst-type dst-x dst-y dst-dir fuel old-dir step plane-type base-time)
  (if (< fuel 0)
      nil
      (let ((path (append path (list (list x y a)))))
  	(if (and
	     (= x dst-x)
	     (= y dst-y)
	     (or (and (eq dst-type 'exit)
		      (= a 9))
		 (and (eq dst-type 'airport)
		      (= a 1)
		      (= dst-dir old-dir)
		      (> (length path) 1)
		      (= (caddr (car (last path 2))) 1)
		      (setf (caddr (car (last path))) 0))))
	    path
	    (if (= fuel 0)
		nil
		(loop
		   for deta-a in (next-a a dst-type (distance x y dst-x dst-y))
		   for p = (loop
			      for i in (next-dirs a x y old-dir dst-x dst-y dst-dir dst-type)
			      for d = (elt (deta-xy) i)
			      for deta-x = (car d)
			      for deta-y = (cadr d)
			      with p = nil
			      if (and (<= 0 (+ deta-a a) 9) (good-dir-p old-dir i) (safe-pos-p (+ x deta-x) (+ y deta-y) (+ a deta-a) (1+ step) plane-type dst-x dst-y dst-type i dst-dir base-time))
			      do (setf p (dfs (+ x deta-x) (+ y deta-y) (+ a deta-a) path dst-type dst-x dst-y dst-dir (1- fuel) i (1+ step) plane-type base-time))
			      until p
			      finally (return p))
		   until p
		   finally (return p)))))))

(defun mark-path (path plane-type base-time)
  (loop
     for p in path
     for step from 0
     for x = (car p)
     for y = (cadr p)
     for a = (caddr p)
     do (loop
	   for time in (step->times step plane-type base-time)
	   do (map-set x y a time))
     ;; do (if (= plane-type 0)
     ;; 	    (progn
     ;; 	      (map-set x y a (* step 2))
     ;; 	      (map-set x y a (1+ (* step 2))))
     ;; 	    (map-set x y a step))
       )
  path)

(defun unmark-path (path plane-type base-time)
  (loop
     for p in path
     for step from 0
     for x = (car p)
     for y = (cadr p)
     for a = (caddr p)
     do (loop
	   for time in (step->times step plane-type base-time)
	   do (map-clr x y a time))
     ;; do (if (= plane-type 0)
     ;; 	    (progn
     ;; 	      (map-clr x y a (* step 2))
     ;; 	      (map-clr x y a (1+ (* step 2))))
     ;; 	    (map-clr x y a step))
       )
  path)

(defun search-dfs (plane-type x y a dst-n dst-type fuel dir step mark-path base-time)
  (let* ((dst (nth dst-n (ecase dst-type
			   (airport (nth 3 *game*))
			   (exit (nth 2 *game*)))))
	 (dst-x (car dst))
	 (dst-y (cadr dst))
	 (dst-dir (dir->num (caddr dst))))
    (if (or (= a 0)
	    (safe-pos-p x y a step plane-type dst-x dst-y dst-type dir dst-dir base-time))
	(let ((p (dfs x y a (make-list step) dst-type dst-x dst-y dst-dir fuel dir step plane-type base-time)))
	  (if mark-path
	      (mark-path p plane-type base-time)
	      p)))))

(defun wait-until-modify (file)
  (with-inotify (inot `((,file ,in-close-write)))
    (read-events inot)))

(defun action (path n)
  (if (nth n path)
      (let* ((o (nth n path))
	     (n (nth (1+ n) path))
	     (o-x (car o))
	     (o-y (cadr o))
	     (x (car n))
	     (y (cadr n))
	     (a (caddr n)))
	(if (and (= o-x x)
		 (= o-y y))
	    nil
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
	     a)))))
(defun plane-a (info)
  (nth 3 info))
(defun plane-type (infos plane-num)
  (loop
     for info in (cdr infos)
     for num = (car info)
     for plane-type = (caadr info)
     until (= num plane-num)
     finally (return plane-type)))
(defun calculate-paths (infos planes base-time &optional depth-count)
  (make-map)
  (mark-all-flying-planes infos base-time)
  (if (hash-table-p planes)
      (progn (setf planes (copy-hash-table planes))
	     (maphash (lambda (k v)
			(mark-path v (plane-type infos k) base-time))
		      planes))
      (setf planes (make-hash-table :test #'equalp)))
  (unmark-destination)
  (loop
     for plane in (cdr infos)
     for num = (car plane)
     for info = (cadr plane)
     unless (gethash num planes)
     do (format t "~a " num) and
     do (unmark-plane num infos base-time) and
     do (setf (gethash num planes)
	      (let* ((plane-type (nth 0 info))
		     (x (nth 1 info))
		     (y (nth 2 info))
		     (dst-n (nth 4 info))
		     (dst-type (nth 5 info))
		     (fuel (nth 6 info))
		     (dir (nth 7 info))
		     (a (plane-a info))
		     (dst (nth dst-n (ecase dst-type
				       (airport (nth 3 *game*))
				       (exit (nth 2 *game*)))))
		     (dst-x (car dst))
		     (dst-y (cadr dst))
		     (dst-dir (dir->num (caddr dst))))
		(a* x y dir a dst-x dst-y dst-dir dst-type fuel plane-type base-time depth-count))) and
     do (unmark-destination))
  planes)
(defun plane-pos (infos plane-num)
  (let ((planes (cdr infos)))
    (loop
       for plane in planes
       until (= (car plane) plane-num)
       finally (return (list (nth 1 (cadr plane))
			     (nth 2 (cadr plane))
			     (nth 3 (cadr plane)))))))
;; (defun every-plane-has-its-path (planes infos time base-time)
;;   (if (hash-table-p planes)
;;       (loop for i in (get-plane-nums infos)
;; 	 always (and
;; 	     (gethash i planes)
;; 	     (let ((pos (nth (time->step time base-time (plane-type infos i)) (gethash i planes))))
;; 	       (if (< (time->step time base-time (plane-type infos i)) (length (gethash i planes)))
;; 		   (if pos
;; 		       (if (equalp pos (plane-pos infos i)) t (return (values nil i)))
;; 		       t)))))))
(defun every-plane-has-its-path (planes infos time base-time)
  (if (hash-table-p planes)
      (let (r)
	(dolist (num (get-plane-nums infos) t)
	  (if (gethash num planes)
	      (let ((pos (nth (time->step time base-time (plane-type infos num)) (gethash num planes))))
		(if (or
		     (>= (time->step time base-time (plane-type infos num)) (1- (length (gethash num planes))))
		     (not (or (= 0 (caddr (plane-pos infos num)))
			      (equalp pos (plane-pos infos num)))))
		    (push num r)))
	      (push num r)))
	(if r
	    (values nil r)
	    t))
      nil))
(defun remove-finish-planes (planes infos time base-time)
  (if (hash-table-p planes)
      (let ((planes-new (make-hash-table :test 'equalp)))
	(loop
	   for i in (get-plane-nums infos)
	   if (gethash i planes)
	   do (when (< (time->step time base-time (plane-type infos i)) (length (gethash i planes)))
		(setf (gethash i planes-new)
		      (gethash i planes))))
	planes-new)))

(defun plane-num->plane-name (num)
  (code-char (+ (char-code #\a) num)))

(defun time->step (time base-time plane-type)
  (ecase plane-type
    (0
     (if (oddp base-time)
	 (truncate (1+ (- time base-time)) 2)
	 (truncate (- time base-time) 2)))
    (1
     (- time base-time))))
(defun next-action (plane-num actions plane-type time base-time)
  (if (and (evenp time) (= plane-type 0))
      ""
      (let ((diff-time
	     (time->step time base-time plane-type)))
	(if (action actions diff-time)
	    (format nil "~aa~a~%~at~a~%"
		    (plane-num->plane-name plane-num) (cadr (action actions diff-time))
		    (plane-num->plane-name plane-num) (car (action actions diff-time)))
	    ;; (format nil "~aa0~%" (plane-num->plane-name plane-num))
	    ""))))
(defun mark-plane (plane-num infos base-time)
  (let* ((info (loop
		  for i in (cdr infos)
		  until (= plane-num (car i))
		  finally (return i)))
	 (plane-type (nth 0 (cadr info)))
	 (x (nth 1 (cadr info)))
	 (y (nth 2 (cadr info)))
	 (a (nth 3 (cadr info))))
    (loop for time in (step->times 0 plane-type base-time)
       do (map-set x y a time))))
(defun unmark-plane (plane-num infos base-time)
  (let* ((info (loop
		  for i in (cdr infos)
		  until (= plane-num (car i))
		  finally (return i)))
	 (plane-type (nth 0 (cadr info)))
	 (x (nth 1 (cadr info)))
	 (y (nth 2 (cadr info)))
	 (a (nth 3 (cadr info))))
    (loop for time in (step->times 0 plane-type base-time)
       do (map-clr x y a time))))
(defun mark-all-flying-planes (infos base-time)
  (loop
     for info in (cdr infos)
     if (> (plane-a (cadr info)) 0)
     do (mark-plane (car info) infos base-time)))
(defun sort-infos-by-plane-type (infos)
  (let ((time (car infos))
	(infos (copy-list (cdr infos))))
    (append (list time) (sort infos (lambda (x y)(> (caadr x) (caadr y)))))))
(defun get-planes-info-a-l (infos base-time)
  (make-map)
  (loop
     for i in (cdr infos)
     for plane-num = (car i)
     for plane-a = (plane-a (cadr i))
     for l = (if (= (plane-type infos plane-num) 1)
		 (length (apply #'search-dfs (append (cadr i) (list 0 nil base-time))))
		 (* 2 (length  (apply #'search-dfs (append (cadr i) (list 0 nil base-time))))))
     collect (list i (if (> plane-a 0) 1 0) l)))
(defun sort-by-planes-shortest-reach-time (infos base-time)
  (append (list (car infos))
	  (mapcar #'car
		  (sort
		   (get-planes-info-a-l (copy-list infos) base-time)
		   (lambda (x y)
		     (if (/= (cadr x) (cadr y))
			 (> (cadr x) (cadr y))
			 (< (caddr x) (caddr y))))))))
(defun sort-by-previous-calculating-result (&optional (infos *infos*) (planes *planes*))
  (let ((time (car infos))
	(planes-info (cdr (copy-list infos))))
    (append (list time)
	    (sort planes-info
		  (lambda (x y)
		    (declare (ignore y))
		    (unless (gethash (car x) planes)
		      t))))))
(defun distance (x1 y1 x2 y2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defun distance-of-dst (plane-info game)
  (let* ((dst-n (nth 4 plane-info))
	 (dst-type (nth 5 plane-info))
	 (dst (nth dst-n (ecase dst-type
			   (airport (nth 3 game))
			   (exit (nth 2 game)))))
	 (dst-x (car dst))
	 (dst-y (cadr dst))
	 (x (nth 1 plane-info))
	 (y (nth 2 plane-info)))
    (+ (expt (- x dst-x) 2)
       (expt (- y dst-y) 2))))
(defun at-exit-p (plane-info game)
  (let ((x (nth 1 plane-info))
	(y (nth 2 plane-info))
	(dsts (nth 2 game)))
    (loop
       for (dst-x dst-y) in dsts
       for r =(and (= x dst-x) (= y dst-y))
       until r
       finally (return r))))
(defun sort-by-if-at-exist (infos game)
  (let ((time (car infos))
	(infos (cdr infos)))
    (loop
       for i from 0 to (1- (length infos))
       until (and (at-exit-p (cadr (nth i infos)) game)
		  (rotatef (nth 0 infos) (nth i infos))))
    (append (list time)
	    infos)))
(defun shuffle-sort-infos (infos)
  (let ((time (car infos))
	(planes (cdr infos)))
    (append `(,time) (shuffle (copy-list planes)))))
(defmacro time-count (var &body body)
  (let ((begin (gensym))
	(end (gensym)))
    `(let ((,begin (get-internal-real-time))
	   (,end (progn ,@body (get-internal-real-time))))
       (incf (nth 0 ,var) (- ,end ,begin))
       (incf (nth 1 ,var)))))
(defmacro max-time (var do-some-thing &body body)
  (let ((begin (gensym))
	(end (gensym)))
    `(let ((,begin (get-internal-real-time))
	   (,end (progn ,@body (get-internal-real-time))))
       (when (> (- ,end ,begin) ,var)
	 (setf ,var (- ,end ,begin))
	 (funcall ,do-some-thing ,var)))))

(defun trim-planes (planes infos time base-time)
  (if (hash-table-p planes)
      (let ((planes-new (make-hash-table :test 'equalp)))
	(maphash (lambda (key value)
		   ;; (if (and (= 0 (plane-type infos key)) (oddp base-time))
		   ;;     (decf base-time))
		   (setf (gethash key planes-new)
			 (nthcdr (time->step time base-time (plane-type infos key))
				 value)))
		 planes)
	planes-new)))

(defun make-a*-map (max-step type &key width height game)
  (if game
      (setf width (car game)
	    height (cadr game)))
  (make-array (list width height max-step 8 10) :element-type type))
(defun available-dirs (pre-dir)
  (loop for i from -2 to 2
     collect (mod (+ pre-dir i) 8)))
(defun dirs->xys (x y dirs)
  (loop for i in (dirs->deta-xys dirs)
     collect (list (+ x (car i)) (+ y (cadr i)))))
(defun available-altitude (pre-a)
  (cond
    ((< 0 pre-a 9) (list (1- pre-a) pre-a (1+ pre-a)))
    ((= pre-a 0) '(1))
    ((= pre-a 9) '(8 9))))
(defun available-neighbors (x y dir a)
  (let* ((dirs (available-dirs dir))
	 (as (available-altitude a)))
    (if (= a 0)
	(list (append (car (dirs->xys x y (list dir))) (list dir (1+ a)))
	      (list x y dir a))
	(loop
	   with r
	   for dir in (dirs->xys x y dirs)
	   for i from 0
	   for x = (car dir)
	   for y = (cadr dir)
	   do (loop for a in as
		 do (push (list x y (nth i dirs) a) r))
	   finally (return r)))))
(defun a*-h (x y dir a dst-x dst-y dst-dir dst-type fuel)
  (declare (ignore fuel dir))
  (ecase dst-type
    (exit
     (let* ((dst-a 9)
	    (cost (max (abs (- x dst-x))
		       (abs (- y dst-y)))))
       (incf cost (- dst-a a))
       cost))
    (airport
     (let*
	 ((dst-a 0)
	  (deta (car (dirs->deta-xys (list (mod (+ 4 dst-dir) 8)))))
	  (dx (car deta))
	  (dy (cadr deta))
	  (cost
	   (if (and (= x (+ dst-x dx))
		    (= y (+ dst-y dy)))
	       (max (abs (- x dst-x))
		    (abs (- y dst-y)))
	       (+ 2 (max (abs (- x (+ dst-x dx)))
			(abs (- y (+ dst-y dy))))))))
       (incf cost (- a dst-a))
       (if (not (backwardp dst-x dst-y dst-dir x y))
	   (incf cost 7))
       ;; (if (and (eq dst-type 'exit)
       ;; 	     (forwardp dst-x dst-y dst-dir x y))
       ;; 	(if (> 2 (min (abs (- x dst-x))
       ;; 		      (abs (- y dst-y))))
       ;; 	    (incf cost 5)))
       ;; (if (eq dst-type 'airport)
       ;; 	(let* ((deta (car (dirs->deta-xys (list (mod (+ 4 dst-dir) 8)))))
       ;; 	       (dx (car deta))
       ;; 	       (dy (cadr deta)))
       ;; 	  (if (forwardp (+ dst-x dx) (+ dst-y dy) dst-dir x y)
       ;; 	      (incf cost (max (abs (- x (+ dst-x dx)))
       ;; 			      (abs (- y (+ dst-y dy))))))))
       cost))))
(defun movement-cost (from-x from-y to-x to-y)
  (max (abs (- from-x to-x))
       (abs (- from-y to-y))))

(defun f-set (f-map x y step dir a f)
  (setf (aref f-map x y step dir a) f))
(defun f-get (f-map x y step dir a)
  (aref f-map x y step dir a))
(defun g-set (g-map x y step dir a g)
  (setf (aref g-map x y step dir a) g))
(defun g-get (g-map x y step dir a)
  (aref g-map x y step dir a))
(defun p-mark (mark-map x y step dir a val)
  (setf (aref mark-map x y step dir a) (1+ val)))
(defun p-unmark (mark-map x y step dir a)
  (setf (aref mark-map x y step dir a) 0))
(defun p-markp (mark-map x y step dir a)
  (< 0 (aref mark-map x y step dir a)))
(defun p-mark-value (mark-map x y step dir a)
  (1- (aref mark-map x y step dir a)))
(defun set-parent (parents-map x y step dir a pre-x pre-y pre-dir pre-a)
  (setf (aref parents-map x y step dir a) (list pre-x pre-y pre-dir pre-a)))
(defun get-parent (parents-map x y step dir a)
  (aref parents-map x y step dir a))

(defun add-p-to-heap (f-map g-map mark-map parents-map x y step dir a f g heap pre-x pre-y pre-dir pre-a flying-p)
  (f-set f-map x y step dir a f)
  (g-set g-map x y step dir a g)
  (p-mark mark-map x y step dir a (nth-value
				   1
				   (add-to-heap heap (list f x y step dir a flying-p))))
  (set-parent parents-map x y step dir a pre-x pre-y pre-dir pre-a))
(defun pop-p-from-heap (mark-map heap)
  (let* ((p (pop-heap heap))
	 (x (nth 1 p))
	 (y (nth 2 p))
	 (step (nth 3 p))
	 (dir (nth 4 p))
	 (a (nth 5 p)))
    (when p
      (p-unmark mark-map x y step dir a))
    p))
(defun delete-p-from-heap (mark-map x y step dir a heap)
  (let ((i (p-mark-value mark-map x y step dir a)))
    (delete-from-heap heap i)))
;; (defun search-dfs (plane-type x y a dst-n dst-type fuel dir step mark-path base-time)
(defun is-dst-p (x y dir a dst-x dst-y dst-dir dst-type)
  (ecase dst-type
    (exit
     (and
      (= x dst-x)
      (= y dst-y)
      (= a 9)))
    (airport
     (and
      (= x dst-x)
      (= y dst-y)
      (= a 0)
      (= dir dst-dir)))))
(defun remove-neighbors (neighbors step plane-type dst-x dst-y dst-type dst-dir base-time)
  (remove-if-not (lambda (p)
		   (let ((x (nth 0 p))
			 (y (nth 1 p))
			 (dir (nth 2 p))
			 (a (nth 3 p)))
		     (safe-pos-p x y a step plane-type dst-x dst-y dst-type dir dst-dir base-time)))
		 neighbors))
(defun a*-construct-path (parents-map x y step dir a)
  (let (path)
    (push (list x y a) path)
    (do
     ((p (get-parent parents-map x y step dir a) (get-parent parents-map x y step dir a)))
     ((= 42 (nth 3 p)) path)
      (setf x (nth 0 p)
	    y (nth 1 p)
	    dir (nth 2 p)
	    a (nth 3 p)
	    step (1- step))
      (push (list x y a) path))))
(defun a* (x y dir a dst-x dst-y dst-dir dst-type fuel plane-type base-time &optional depth-count make-map)
  (when make-map (make-map))
  (let ((heap (make-instance 'binary-heap :key 'car))
	(parents-map (make-a*-map *max-step* t :game *game*))
	(f-map (make-a*-map *max-step* 'signle-float :game *game*))
	(mark-map (make-a*-map *max-step* 'signle-float :game *game*))
	(g-map (make-a*-map *max-step* 'signle-float :game *game*)))
    (add-p-to-heap f-map g-map mark-map parents-map x y 0 dir a (a*-h x y dir a dst-x dst-y dst-dir dst-type fuel) 0 heap x y dir 42 nil)
    (do
     ((p (pop-p-from-heap mark-map heap) (pop-p-from-heap mark-map heap))
      r
      findp
      ps
      (count 0 (1+ count)))
     (findp r)
      (let ((pre-x (nth 1 p))
	    (pre-y (nth 2 p))
	    (pre-step (nth 3 p))
	    (pre-dir (nth 4 p))
	    (pre-a (nth 5 p))
	    (flying-p (nth 6 p)))
	(push (list pre-x pre-y pre-step pre-dir pre-a) ps)
	(if (or flying-p (> pre-a 0))
	    (setf flying-p t))
	;; (setf r (remove-neighbors (available-neighbors pre-x pre-y pre-dir pre-a)
	;; 			  (1+ pre-step) plane-type dst-x dst-y dst-type dst-dir base-time))
	(dolist (p (if flying-p
		       (remove-neighbors (available-neighbors pre-x pre-y pre-dir pre-a)
					 (1+ pre-step) plane-type dst-x dst-y dst-type dst-dir base-time)
		       (append (remove-neighbors (available-neighbors pre-x pre-y pre-dir pre-a)
						 (1+ pre-step) plane-type dst-x dst-y dst-type dst-dir base-time)
			       (list (list pre-x pre-y pre-dir pre-a)))))
	  (let* ((x (nth 0 p))
		 (y (nth 1 p))
		 (dir (nth 2 p))
		 (a (nth 3 p))
		 (step (1+ pre-step))
		 (g (1+ (g-get g-map pre-x pre-y pre-step pre-dir pre-a)))
		 (h (a*-h x y dir a dst-x dst-y dst-dir dst-type fuel))
		 (f (+ g (* 3 h) (random 0.1))))
	    (if (is-dst-p x y dir a dst-x dst-y dst-dir dst-type)
		(progn
		  (set-parent parents-map x y step dir a pre-x pre-y pre-dir pre-a)
		  (setf findp t r (a*-construct-path parents-map x y step dir a)))
		(progn
		  (when (or (= 0 (f-get f-map x y step dir a))
			    (< f (f-get f-map x y step dir a)))
		    (when (p-markp mark-map x y step dir a)
		      (delete-p-from-heap mark-map x y step dir a heap)))
		  (unless (p-markp mark-map x y step dir a)
		    (add-p-to-heap f-map g-map mark-map parents-map x y step dir a f g heap pre-x pre-y pre-dir pre-a flying-p)
		    (if depth-count
			(incf (aref depth-count x y))))))))
	))))

(defun main (in-file out-file &optional break-time)
  (setf *ct* (ct-init))
  (with-open-file (log "/tmp/atc-log" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-open-file (out out-file :direction :output :if-exists :append :if-does-not-exist :create :external-format :latin-1)
      (loop
	 with base-time = 0 and planes
	 for infos = (progn (wait-until-modify in-file)
			    (get-infos in-file))
	 for time = (car infos)
	 do (format log "~a~%~a~%" (get-internal-real-time) infos)
	 do (setf *a* (list infos planes base-time time))
	 unless (every-plane-has-its-path planes (copy-list infos) time base-time)
	 do (handler-case
		(with-timeout (1.5)
		  (setf planes (trim-planes (remove-finish-planes planes (copy-list infos) time base-time)
					    (copy-list infos) time base-time)
			base-time time
			planes (ct-count *ct* (calculate-paths (copy-list infos) planes base-time)))))
	 else
	 do (sb-ext:gc :full t)
	 unless (every-plane-has-its-path planes (copy-list infos) time base-time)
	 ;; do (setf infos (sort-by-if-at-exist infos *game*)) and
	 do (handler-case
	 	(with-timeout (1)
	 	  (setf	base-time time
	 		planes (calculate-paths (copy-list infos) nil base-time))))
	 unless (every-plane-has-its-path planes (copy-list infos) time base-time)
	 do (error "not all have path")
	 do (av:make-map *game* infos (trim-planes (remove-finish-planes planes (copy-list infos) time base-time) (copy-list infos) time base-time)
			 (format nil "/dev/shm/atc/atc-~a.png" time))
	 do (progn
	      #+sbcl (sb-ext:run-program "ln" (list "-fs" (format nil "/dev/shm/atc/atc-~a.png" time) "/dev/shm/atc.png") :search t))
	 do (loop
	       for plane-num in (get-plane-nums infos)
	       for actions = (gethash plane-num planes)
	       if actions
	       do (princ (princ (next-action plane-num actions (plane-type infos plane-num) time base-time)
				out) log))
	 do (finish-output out)
	 do (finish-output log)
	 if (equalp break-time time)
	 do (error "break-time")))))
;; (defun test ()
;;   (let*
;;       ((infos (get-infos "/dev/shm/atc-out"))
;;        (planes (calculate-paths infos nil (car infos)))
;;        (time (car infos)))
;;     (loop
;;        for plane-num in (get-plane-nums infos)
;;        for actions = (gethash plane-num planes)
;;        if actions
;;        do (princ (next-action plane-num actions (plane-type infos plane-num) time time)))))
