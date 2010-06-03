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
               :eos
               :bordeaux-threads
               :cl-irc
               :closer-mop
               :nisp.network-tree

               #+ () :nisp.global
               #+ () :nisp.mop
               #+ () :drakma
               #+ () :cl-github
               #+ () :closure-html
               ;; Still required by commands/wikihow.lisp
               #+ () :cl-l10n)
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
