;; system definition file
(in-package :cl-user)
(defpackage #:nispbot-system
  (:use :cl :asdf))
(in-package #:nispbot-system)

(defsystem :nisp
  :version "0.0.1"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "Nixeagle's random lisp experiments"
  :depends-on (:cl-ppcre
               :lift)
  :serial t
  :components
  ((:file "main-test-suite")
   (:module "random"
            :components
            ((:file "package")
             (:file "util")
             (:file "defconstant")
             (:file "random")))
   (:file "package")
   (:file "nisp-asdf")
   (:module "safe"
            :components
            ((:file "package")
             (:file "empty-package")
             (:file "safe")))))

(defsystem :nispbot
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "irc bot"
  :depends-on (:lift
               :cl-ppcre
               :cl-irc
               :nisp)
  :serial t
  :components
  ((:file "nisp-introspect")
   (:file "config-dist")
   (:file "config")
   (:file "safe-scope")
   (:file "nispbot")))