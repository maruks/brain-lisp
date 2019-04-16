(defpackage :brain-server
  (:use :common-lisp :websocket-driver :clack :lack :cl-json :brain)
  (:export :start :halt :reset :main))

(in-package :brain-server)

;; dependencies
;; (ql:quickload '(:websocket-driver :clack :cl-json :hunchentoot))
;; (ql:quickload "brain")

(defvar *handler* nil)

(defvar *state* nil)

(defparameter *static*
  (lack:builder
   (:static :path (lambda (path) (if (string= "/" path) "/index.html" path))
	    :root (asdf:system-relative-pathname 'brain "public/"))
   (lambda (env)
     (declare (ignore env))
     '(200 nil nil))))

(defun colors-msg ()
  (let* ((on-color (acons :code 1 (list (cons :color "#0875d0"))))
	 (default-color (acons :code 0 (list (cons :color "#e6e6e6"))))
	 (dying-color (acons :code 2 (list (cons :color "#6EAFE6"))))
	 (colors (list default-color on-color dying-color)))
    (acons :tag :colors (list (cons :colors colors)))))

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
		   (:started (progn (send ws (encode-json-alist-to-string (grid->list grid)))
				    (next-grid grid (afield :width client) (afield :height client))))
		   (t (send ws (encode-json-alist-to-string '((:tag . :error) (:code . 1))))))))))))

(defun client-id (env)
  (format nil "~s ~d" (getf env :remote-addr) (getf env :remote-port)))

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

(defun main (&rest args)
  (declare (ignore args))
  (start))
