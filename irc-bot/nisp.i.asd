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
               :nisp.network-tree
               :eos
               :bordeaux-threads
               :trivial-timers

               :split-sequence
               :anaphora

               :cl-ppcre
               :alexandria
               :cl-irc
               :iterate
               :closer-mop

               :drakma
               :cl-github
               :closure-html
               :cl-l10n)
  :serial t
  :components
  ((:file "package")
   (:file "superclass-helpers")         ;in use by ADD-IRC-USER-SUPERCLASS
   (:file "comchar")
   (:module "irc"
            :components
            ((:file "connect")))
   (:file "bot")
  #+ () (:module "commands"
            :components
            ((:file "links")
             (:file "github")
             (:file "beta")))
  #+ () (:file "user-init")
   #+ () (:file "tests")))

;;; END
