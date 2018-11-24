(defpackage :brain
  (:use :common-lisp :websocket-driver :clack :lack :cl-json)
  (:export :start :halt :reset))

;; https://en.wikipedia.org/wiki/Brian%27s_Brain
;; off, 2 on -> on
;; on -> dying
;; dying -> off

(in-package :brain)

;; (ql:quickload '(:websocket-driver :clack :cl-json :hunchentoot))

;; create manifest file
;; (asdf:operate 'asdf:load-op 'brain)
;; (ql:write-asdf-manifest-file "quicklisp-manifest.txt")

(defvar *handler* nil)

(defvar *state* nil)

(defparameter *static*
  (lack:builder
   (:static :path (lambda (path) (if (string= "/" path) "/index.html" path))
	    :root #P"public/")
   (lambda (env)
     (declare (ignore env))
     '(200 nil nil))))

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
      (:off (if (eq 2 (count-live-neighbours grid (car point) (cdr point))) :on :off)))))

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

(defun grid-to-list (grid)
  (let ((cells (loop for point being the hash-keys of grid
		  collect (acons :color (if (eq :on (gethash point grid)) 1 2) (list (list :point (car point) (cdr point)))))))
    (acons :tag :cells (list (cons :cells cells)))))

(defun colors-msg ()
  (let* ((on-color (acons :code 1 (list (cons :color "#0875d0"))))
	 (default-color (acons :code 0 (list (cons :color "#e6e6e6"))))
	 (dying-color (acons :code 2 (list (cons :color "#6EAFE6"))))
	 (colors (list default-color on-color dying-color)))
    (acons :tag :colors (list (cons :colors  colors)))))

(defun afield (field alist &optional (test #'eq))
  (cdr (assoc field alist :test test)))

(defun ensure-initialized (client-id)
  (unless (afield client-id *state* #'string=)
      (setq *state* (acons client-id (list '(:state :initial)) *state*))))

(defun handle-msg (message client-id ws)
  (ensure-initialized client-id)
  (destructuring-bind (tag . payload) (car message)
    (let ((client (afield client-id *state* #'string=)))
      (case tag
	(:start (let* ((grid (new-grid (afield :width payload) (afield :height payload)))
		       (cons-state (acons :grid grid (acons :state :started payload))))
		  (setf (cdr (assoc client-id *state* :test #'string=)) cons-state)
		  (send ws (encode-json-alist-to-string (colors-msg)))))
	(:next (let* ((state (afield :state client))
		      (grid (afield :grid client)))

		 (case state
		   (:started (progn (send ws (encode-json-alist-to-string (grid-to-list grid)))
				    (next-grid grid (afield :width client) (afield :height client))))
		   (t (send ws (encode-json-alist-to-string '((:tag . :error) (:code . 1))))))))))))

(defun client-id (env)
  (format nil "~s ~d" (getf env :remote-addr) (getf env :remote-port)))

(defun count-live-neighbours (grid x y)
  (let* ((ps (apply #'append (loop for dx from -1 to 1
				collect (loop for dy from -1 to 1
					   collect (cons (+ x dx) (+ y dy))) )))
	 (ns (remove (cons x y) ps :test #'equal)))
    (loop for p in ns
       counting (eq :on (gethash p grid)))))

(defparameter *server*
  (lambda (env)
    (cond
      ((equal (getf env :path-info) "/websocket")
       (let ((ws (make-server env)))

	 (on :open ws
	     (lambda ()
	       (setq *state* (acons (client-id env) (list '(:state :initial)) *state*))))

	 (on :message ws
	     (lambda (message)
	       (handle-msg (decode-json-from-string message) (client-id env) ws)))

	 (on :close ws
	     (lambda (&key code reason)
	       (declare (ignore code))
	       (declare (ignore reason))
	       (setq *state* (remove (client-id env) *state* :test (lambda (e c) (string= (car c) e))))))

	 (lambda (responder)
	   (declare (ignore responder))
	   (start-connection ws))))

      (t (funcall *static* env)))))

(defun start ()
  (unless *handler*
    (setq *handler*
	  (clack:clackup *server* :port 8080))
    (setq *state* nil)))

(defun halt ()
  (when *handler*
    (clack:stop *handler*)
    (setq *handler* nil)))

(defun reset ()
  (halt)
  (start))
