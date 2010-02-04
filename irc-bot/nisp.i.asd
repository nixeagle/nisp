(in-package :cl-user)

(defpackage #:nisp.i-system
  (:use :cl :asdf))

(in-package #:nisp.i-system)

(defsystem :nisp.i
  :version "0.0.0"
  :license "GPLv3 or later"
  :description "irc bot"
  :depends-on (:split-sequence
               :alexandria
               :cl-irc
               :iterate
               :closer-mop)
  :serial t
  :components
  ((:file "package")
   (:file "bot-types")
   (:file "bot")))