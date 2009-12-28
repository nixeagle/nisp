;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10;  indent-tabs-mode: nil -*-
(in-package :cl-user)
(defpackage #:functional-tests-system
  (:use :cl :asdf))
(in-package #:functional-tests-system)

(defsystem :functional-tests
  :version "0.0.9"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "Testing by the function"
  :serial t
  :components
  ((:file "util")
   (:file "functional-tests" :depends-on ("util"))))

(defpackage #:functional-tests
  (:use :cl))