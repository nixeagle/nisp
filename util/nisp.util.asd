(in-package :cl-user)

(defpackage #:nisp.util-system
  (:use :cl :asdf))

(in-package #:nisp.util-system)

(defsystem :nisp.util
  :version "0.0.1"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv3 or later"
  :description "irc bot"
  :depends-on (:iterate :eos :alexandria :closer-mop)
  :components
  ((:file "nisp-standard-combination")))

;;; END