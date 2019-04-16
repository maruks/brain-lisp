;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:brain-asd
  (:use :cl :asdf))

(in-package :brain-asd)

(asdf:defsystem "brain"
  :name "brain"
  :description "cellular automaton brain"
  :version "0.0.1"
  :author "Maris Orbidans"
  :licence "Public Domain"

  :depends-on (:hunchentoot
	       :websocket-driver
	       :clack
	       :clack-handler-hunchentoot
	       :cl-json)

  :serial t
  :entry-point "brain-server:main"
  :components ((:module "src"
		:serial t
		:components ((:file "brain")
			     (:file "brain-server")))
	       (:module "public"
		:components ((:static-file "elm.js")
			     (:static-file "index.html")
			     (:static-file "style.css"))))
  :in-order-to ((test-op (test-op "brain/tests"))))

(asdf:defsystem "brain/tests"
  :licence "Public Domain"
  :depends-on (:brain
	       :alexandria
	       :fiasco)
  :serial t
  :components ((:module "tests"
		:components ((:file "brain-tests"))))
  :perform (test-op (o c) (uiop:symbol-call 'fiasco 'all-tests)))
