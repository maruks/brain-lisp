(defpackage :brain
  (:use :common-lisp)
  (:export :initialize-grid :new-grid :next-state :next-grid :grid->list :count-live-neighbours))

(in-package :brain)

(defun initialize-grid (grid width height)
  (loop
    :for x :from 0 :to (1- width)
    :do (loop
	  :for y :from 0 :to (1- height)
	  :do (when (zerop (random 4))
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
      (let* ((points-states (loop :for x :from 0 :to (1- width)
				  :append (loop :for y :from 0 :to (1- height)
						:for point = (cons x y)
						:for state = (next-state grid point)
						:if (not (eq :off state))
						:collect (list point state)))))
	(clrhash grid)
	(loop :for (point state) :in points-states
	      :do (setf (gethash point grid) state))
	grid)))

(defun grid->list (grid)
  (let ((cells (loop :for point :being :the :hash-keys :of grid
		     :for color = (if (eq :on (gethash point grid)) 1 2)
		     :for point-xy = (list (list :point (car point) (cdr point)))
		     :collect (acons :color color point-xy))))
    (acons :tag :cells (list (cons :cells cells)))))

(defun count-live-neighbours (grid x y)
  (loop :for dx :from -1 :to 1
	:sum (loop :with center = (cons x y)
		   :for dy :from -1 :to 1
		   :for point = (cons (+ x dx) (+ y dy))
		   :count (and (eq :on (gethash point grid)) (not (equal point center))))))
