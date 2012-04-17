(declaim (optimize (debug 3)))

(defpackage :cfy.atc-bot
  (:use :cl :alexandria :inotify))
(in-package :cfy.atc-bot)
(defvar *game* nil)
(defparameter *max-time* 100)
(defvar *infos* nil)
(defvar *planes* nil)
(defvar *time-start* nil)
;; (defvar *actions* (make-hash-table))
(defvar *map* nil)
(defvar *map2* nil)
(defparameter *deta-xy*
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
     collect (aref *deta-xy* i)))

(defun forwardp (x0 y0 dir x1 y1)
  (ecase dir
    (0 (> y0 y1))
    (2 (< x0 x1))
    (4 (< y0 y1))
    (6 (> x0 x1))))
(defun backwardp (x0 y0 dir x1 y1)
  (ecase dir
    (0 (< y0 y1))
    (2 (> x0 x1))
    (4 (> y0 y1))
    (6 (< x0 x1))))
(defun parrelp (x0 y0 dir x1 y1)
  (ecase dir
    (0 (= y0 y1))
    (2 (= x0 x1))
    (4 (= y0 y1))
    (6 (= x0 x1))))
(defun leftp (x0 y0 dir x1 y1)
  (ecase dir
    (0 (> x0 x1))
    (2 (> y0 y1))
    (4 (< x0 x1))
    (6 (< y0 y1))))
(defun rightp (x0 y0 dir x1 y1)
  (ecase dir
    (0 (< x0 x1))
    (2 (< y0 y1))
    (4 (> x0 x1))
    (6 (> y0 y1))))
(defun middlep (x0 y0 dir x1 y1)
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
	 (get-dirs (next-dirs-airport x y dir dst-x dst-y dst-dir) dir))
	(exit
	 (next-dirs-exit x y dst-x dst-y dst-dir)))))
(defun next-dirs-airport (x y dir dst-x dst-y dst-dir)
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
	    (sr '(hr s hl))
	    (sl '(hr s hl))))
	 (t
	  '(s))))
      (db
       (cond
	 (dl
	  (if sf
	      (if sl
		  '(s hl hr)
		  '(hr hl s))
	      '(hl s hr)))
	 (dr
	  (if sf
	      (if sr
		  '(s hr hl)
		  '(hl hr s))
	      '(hr s hl)))
	 (dm
	  (cond
	    (sf '(s hl hr))
	    (sb '(hl hr s))
	    (sr '(hr s hl))
	    (sl '(hl s hr)))))))))
(defun next-dirs-exit (x y dst-x dst-y dst-dir)
  (let ((d-x dst-x) (d-y dst-y))
    (ecase dst-dir
      (4 (incf d-y))
      (5 (decf d-x) (incf d-y))
      (6 (decf d-x))
      (7 (decf d-x) (decf d-y))
      (0 (decf d-y))
      (1 (incf d-x) (decf d-y))
      (2 (incf d-x))
      (3 (incf d-x) (incf d-y)))
    (if (and (= d-x x) (= d-y y))
	(list (position (list (- dst-x x)
			      (- dst-y y))
			*deta-xy* :test #'equalp))
	(cond
	  ((and (> d-x x) (> d-y y)) '(3 1 5 2 4 0 6 7))
	  ((and (> d-x x) (= d-y y)) '(2 0 4 1 3 5 7 6))
	  ((and (> d-x x) (< d-y y)) '(1 7 3 0 2 6 4 5))
	  ((and (= d-x x) (> d-y y)) '(4 5 3 6 2 7 1 0))
	  ((and (= d-x x) (< d-y y)) '(0 7 1 6 2 5 3 4))
	  ((and (< d-x x) (> d-y y)) '(5 6 4 7 3 0 2 1))
	  ((and (< d-x x) (= d-y y)) '(6 7 5 0 4 1 3 2))
	  ((and (< d-x x) (< d-y y)) '(7 0 6 1 5 2 4 3))))))
(defun good-dir-p (old-dir new-dir)
  (if (or
       (<= (mod (- old-dir new-dir) 8)2)
       (<= (mod (- new-dir old-dir) 8) 2))
      t))
(defun good-pos-p (x y)
  (and (< -1 x (car *game*))
       (< -1 y (cadr *game*))
       (aref *map* x y 0)))
(defun next-a-exit (a)
  (cond
    ((< 1 a 9)
     '(1 0 -1))
    ((= a 9)
     '(0 -1))
    ((= a 1)
     '(1 0))
    (t
     '(1))))
(defun next-a-airport (a)
  (cond
    ((< 1 a 9)
     '(-1 0 1))
    ((= a 9)
     '(-1 0))
    ((= a 1)
     '(0 1))
    (t
     '(1))))

(defun next-a (a dst-type)
  (ecase dst-type
    (airport (next-a-airport a))
    (exit (next-a-exit a))))

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
	 (map (make-array (list width height *max-time*))))
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
       for n from 0
       for x = (car i)
       for y = (cadr i)
       do (loop for time from 0 to (1- *max-time*)
	     do (setf (aref map x y time) 0)))
    (loop
       for i in airports
       for n from 0
       for x = (car i)
       for y = (cadr i)
       do (loop for time from 0 to (1- *max-time*)
	     do (setf (aref map x y time) 0)))
    (setf *map* map)
    t))
(defun safe-pos-p (x y a step plane-type dst-x dst-y dst-type)
  (if (and (= x dst-x)
	   (= y dst-y)
	   (ecase dst-type
	     (exit (= a 9))
	     (airport (= a 0))))
      t
      (if (and
	   (good-pos-p x y)
	   (not (null (aref *map* x y step)))
	   (loop for time in (if (= plane-type 0) `(,(* 2 step) ,(+ 1 step step)) `(,step))
	      always (loop for deta-a in '(-1 0 1)
			always (loop
				  for j from 0
				  for i across *deta-xy*
				  for deta-x = (car i)
				  for deta-y = (cadr i)
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

(defun get-plane-nums (&optional infos)
  (unless infos (setf infos *infos*))
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
(defun fix-dir (dir)
  (ecase dir
    ((1 3 5 7) '(0 2 4 6))
    ((0 2 4 6))))

(defun dfs (x y a path dst-type dst-x dst-y dst-dir fuel old-dir step plane-type)
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
		      (= (caddr (car (last path 2))) 1)
		      (setf (caddr (car (last path))) 0))))
	    path
	    (if (= fuel 0)
		nil
		(loop
		   for deta-a in (next-a a dst-type)
		   for p = (loop
			      with p = nil
			      for i in (if (and (eq dst-type 'airport)
						(member old-dir '(1 3 5 7)))
					   (fix-dir old-dir)
					   (next-dirs a x y old-dir dst-x dst-y dst-dir dst-type))
			      for d = (elt *deta-xy* i)
			      for deta-x = (car d)
			      for deta-y = (cadr d)
			      if (and (<= 0 (+ deta-a a) 9) (good-dir-p old-dir i) (safe-pos-p (+ x deta-x) (+ y deta-y) (+ a deta-a) (1+ step) plane-type dst-x dst-y dst-type))
			      do (setf p (dfs (+ x deta-x) (+ y deta-y) (+ a deta-a) path dst-type dst-x dst-y dst-dir (1- fuel) i (1+ step) plane-type))
			      until p
			      finally (return p))
		   until p
		   finally (return p)))))))

(defun mark-path (path plane-type)
  (loop
     for p in path
     for step from 0
     for x = (car p)
     for y = (cadr p)
     for a = (caddr p)
     do (if (= plane-type 0)
	    (progn
	      (map-set x y a (* step 2))
	      (map-set x y a (1+ (* step 2))))
	    (map-set x y a step)))
  path)

(defun unmark-path (path plane-type)
  (loop
     for p in path
     for step from 0
     for x = (car p)
     for y = (cadr p)
     for a = (caddr p)
     do (if (= plane-type 0)
	    (progn
	      (map-clr x y a (* step 2))
	      (map-clr x y a (1+ (* step 2))))
	    (map-clr x y a step)))
  path)

(defun search-dfs (plane-type x y a dst-n dst-type fuel dir &optional (step 0))
  (let* ((dst (nth dst-n (ecase dst-type
			     (airport (nth 3 *game*))
			     (exit (nth 2 *game*)))))
	 (dst-x (car dst))
	 (dst-y (cadr dst))
	 (dst-dir (dir->num (caddr dst))))
    (if (safe-pos-p x y a step plane-type dst-x dst-y dst-type)
	(mark-path (dfs x y a (make-list step) dst-type dst-x dst-y dst-dir fuel dir step plane-type) plane-type))))

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
	 a))))
(defun plane-a (info)
  (nth 3 info))
(defun plane-type (infos plane-num)
  (loop
     for info in (cdr infos)
     for num = (car info)
     for plane-type = (caadr info)
     until (= num plane-num)
     finally (return plane-type)))
(defun calculate-paths (&optional (infos *infos*))
  (make-map)
  (mark-all-flying-planes infos)
  (setf *planes* (make-hash-table :test #'equalp))
  (loop
     for plane in (cdr infos)
     for num = (car plane)
     for info = (cadr plane)
     do (unmark-plane num infos)
     do (setf (gethash num *planes*)
	      (if (= 0 (plane-a info))
		  (let ((plane-type (nth 0 info))
			(x (nth 1 info))
			(y (nth 2 info))
			(dst-n (nth 4 info))
			(dst-type (nth 5 info))
			(fuel (nth 6 info))
			(dir (nth 7 info)))
		    (loop
		       for step from 0
		       for i = (search-dfs plane-type x y 0 dst-n dst-type fuel dir step)
		       until i
		       finally (return i)))
		  (apply #'search-dfs info)))))
(defun every-plane-has-its-path (&optional planes infos)
  (and (if planes planes *planes*)
       (loop for i in (get-plane-nums (if infos infos *infos*))
	  always (gethash i (if planes planes *planes*)))))

;; (defun get-path (in-file)
;;   (make-map)
;;   (loop
;;      for i in (get-planes in-file)
;;      for plane = (code-char (+ (char-code #\a) (car i)))
;;      for info = (cadr i)
;;      for path = (apply #'search-dfs info)
;;      collect (list plane path)))

;; (defun get-next-actions (in-file)
;;   (make-map)
;;   (loop
;;      for i in (get-planes in-file)
;;      for plane = (code-char (+ (char-code #\a) (car i)))
;;      for info = (cadr i)
;;      for action = (action (apply #'search-dfs info))
;;      collect (format nil "~aa~a~%~at~a~%" plane (cadr action) plane (car action))))


(defun plane-num->plane-name (num)
  (code-char (+ (char-code #\a) num)))
(defun next-action (plane-num actions plane-type time1 time2)
  (let ((diff-time
	 (ecase plane-type
	   (0 (truncate (- time1 time2) 2))
	   (1 (- time1 time2)))))
    (if (car (action actions diff-time))
	(format nil "~aa~a~%~at~a~%"
		(plane-num->plane-name plane-num) (cadr (action actions diff-time))
		(plane-num->plane-name plane-num) (car (action actions diff-time)))
	"")))
(defun mark-plane (plane-num &optional infos)
  (unless infos
    (setf infos *infos*))
  (let* ((info (loop
		 for i in (cdr infos)
		 until (= plane-num (car i))
		 finally (return i)))
	 (plane-type (nth 0 (cadr info)))
	 (x (nth 1 (cadr info)))
	 (y (nth 2 (cadr info)))
	 (a (nth 3 (cadr info))))
    (if (= plane-type 0)
	(progn
	  (map-set x y a 0)
	  (map-set x y a 1))
	(map-set x y a 0))))
(defun unmark-plane (plane-num &optional infos)
  (unless infos
    (setf infos *infos*))
  (let* ((info (loop
		 for i in (cdr infos)
		 until (= plane-num (car i))
		 finally (return i)))
	 (plane-type (nth 0 (cadr info)))
	 (x (nth 1 (cadr info)))
	 (y (nth 2 (cadr info)))
	 (a (nth 3 (cadr info))))
    (if (= plane-type 0)
	(progn
	  (map-clr x y a 0)
	  (map-clr x y a 1))
	(map-clr x y a 0))))
(defun mark-all-flying-planes (&optional (infos *infos*))
  (loop
     for info in (cdr infos)
     if (> (plane-a (cadr info)) 0)
     do (mark-plane (car info) infos)))

(defun main (in-file out-file)
  (setf *time-start* 0 *planes* nil)
  (with-open-file (log "/dev/shm/atc-log" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-open-file (out out-file :direction :output :if-exists :append :if-does-not-exist :create)
      (loop
	 for infos = (progn (wait-until-modify in-file) (get-infos in-file))
	 for time = (car infos)
	 do (progn
	      (unless (every-plane-has-its-path *planes* infos)
		(calculate-paths infos)
		(setf *time-start* time))
	      (format log "~a~%~a~%~a~%" *time-start* infos (hash-table-plist *planes*))
	      (loop
		 for plane-num in (get-plane-nums infos)
		 for actions = (gethash plane-num *planes*)
		 ;; for plane-num being the hash-key of *planes* using (hash-value actions)
		 if actions
		 do (princ (princ (next-action plane-num actions (plane-type infos plane-num) time *time-start*)
				  out)
			   log) and
		 do (finish-output out) and
		 do (finish-output log)))))))
