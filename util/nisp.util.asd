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
  :depends-on (:iterate :eos :with-fbound :alexandria)
  :components
  ((:file "declarations")
   (:file "types")
   (:file "util-protocol")
   (:file "tests" :depends-on ("declarations"))))