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

  :depends-on (:hunchentoot :websocket-driver :clack :clack-handler-hunchentoot :cl-json)

  :components ((:file "brain")))
