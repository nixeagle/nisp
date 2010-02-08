(in-package :cl-user)

(defpackage #:nisp.i-system
  (:use :cl :asdf))

(in-package #:nisp.i-system)

(defsystem :nisp.i
  :version "0.0.0"
  :license "GPLv3 or later"
  :description "irc bot"
  :depends-on (:eos
               :with-fbound
               :split-sequence
               :alexandria
               :cl-irc
               :iterate
               :closer-mop)
  :serial t
  :components
  ((:file "util.packages")
   (:file "package")
   (:file "irc-packages")
   (:file "bot-types")
   (:file "symbols")
   (:file "bot"))) 