(in-package :cl-user)
(defpackage #:functional-tests-system
  (:use :cl :asdf))
(in-package #:functional-tests-system)
(defpackage #:functional-tests-system
  (:use :cl :asdf))

(defsystem :functional-tests
  :version "0.0.9"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "Testing by the function"
  :serial t
  :components
  ((:file "functional-tests")))
