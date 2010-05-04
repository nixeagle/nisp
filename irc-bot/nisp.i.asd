(in-package :cl-user)

(defpackage #:nisp.i-system
  (:use :cl :asdf))

(in-package #:nisp.i-system)
(defsystem :nisp.core
  :description "Base of the bot."
  :depends-on (:nisp.network-tree
               :closer-mop)
  :serial t
  :components
  ((:file "core-package")
   (:file "mixin")
   (:file "nisp-handle-command-method")))

(defsystem :nisp.i
  :version "0.0.0"
  :license "GPLv3 or later"
  :description "irc bot"
  :depends-on (:nutils
               :nisp.core
               :nisp.mop
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
  ((:file "package")
   (:file "superclass-helpers")         ;in use by ADD-IRC-USER-SUPERCLASS
   (:file "mixin")
   (:file "irc-packages")          ;Out of date/not used.
   (:file "comchar")
   (:file "symbols")
   (:file "nisp-handle-command-method") ;major logic
   (:module "irc"
            :components
            ((:file "connect")))
   (:file "bot")
   (:module "commands"
            :components
            ((:file "links")
             (:file "github")
             (:file "beta")))
   (:file "user-init")
   (:file "tests")))