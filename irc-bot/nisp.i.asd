(in-package :cl-user)

(defpackage #:nisp.i-system
  (:use :cl :asdf))

(in-package #:nisp.i-system)

(defsystem :nisp.i
  :version "0.0.0"
  :license "GPLv3 or later"
  :description "irc bot"
  :depends-on (:nisp.mop
               :nisp.global
               :nisp.util
               :nisp.network-tree
               :eos
               :bordeaux-threads
               :trivial-timers
               :cl-github
               :closure-html
               :cl-l10n
               :nisp.util.usocket
               :split-sequence
               :anaphora
               :drakma
               :cl-ppcre
               :english-parser
               :acumen
               :alexandria
               :cl-irc
               :iterate
               :closer-mop)
  :serial t
  :components
  (
   (:file "util.packages")        ;Out of date/not used.
   (:file "package")
   (:file "superclass-helpers")
   (:file "mixin")
   (:file "route-call-times")
   (:file "irc-packages")          ;Out of date/not used.
   (:file "comchar")
   (:file "symbols")
   (:file "nisp-handle-command-method")
   (:module "irc"
            :components
            ((:file "connect")))
   (:file "bot")
   (:module "commands"
            :components
            ((:file "github")
             (:file "beta")))
   (:file "user-init")
   (:file "tests")))