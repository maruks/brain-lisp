(defpackage :brain
  (:use :common-lisp)
  (:export :initialize-grid :new-grid :next-state :next-grid :grid->list :count-live-neighbours))

(in-package :brain)

(defun initialize-grid (grid width height)
  (loop
     for x from 0 to (1- width)
     do (loop
	   for y from 0 to (1- height)
	   do (when (zerop (random 4))
		(setf (gethash (cons x y) grid) (if (zerop (random 2)) :on :dying)))))
  grid)

(defun new-grid (width height)
  (let ((grid (make-hash-table :size (* width height) :test #'equal)))
    (initialize-grid grid width height)))

(defun next-state (grid point)
  (let ((state (gethash point grid :off)))
    (case state
      (:on :dying)
      (:dying :off)
      (:off (if (= 2 (count-live-neighbours grid (car point) (cdr point))) :on :off)))))

(defun next-grid (grid width height)
  (if (zerop (hash-table-count grid))
      (initialize-grid grid width height)
      (let* ((points (apply #'append (loop for x from 0 to (1- width)
					collect (loop for y from 0 to (1- height)
						   collect (cons x y)))))
	     (states (mapcar (lambda (p) (next-state grid p)) points)))
	(clrhash grid)
	(loop for p in points
	   for s in states
	   do (unless (eq :off s)
		(setf (gethash p grid) s)))
	grid)))

(defun grid->list (grid)
  (let ((cells (loop for point being the hash-keys of grid
		  collect (acons :color (if (eq :on (gethash point grid)) 1 2) (list (list :point (car point) (cdr point)))))))
    (acons :tag :cells (list (cons :cells cells)))))

(defun count-live-neighbours (grid x y)
  (let* ((ps (apply #'append (loop for dx from -1 to 1
				collect (loop for dy from -1 to 1
					   collect (cons (+ x dx) (+ y dy))) )))
	 (ns (remove (cons x y) ps :test #'equal)))
    (loop for p in ns
       counting (eq :on (gethash p grid)))))
