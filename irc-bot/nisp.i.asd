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
               :nisp.util.usocket
               :split-sequence
               :alexandria
               :cl-irc
               :nisp.mop
               :iterate
               :closer-mop)
  :serial t
  :components
  ((:file "util.packages")
   (:file "package")
   (:file "mixin")
   (:file "irc-packages")
   (:file "bot-types")
   (:file "symbols")
   (:file "bot")
   (:file "i.fbi")
   (:file "tests")))