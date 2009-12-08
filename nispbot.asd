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
  :weakly-depends-on (:Fiveam)
  :depends-on (:cl-ppcre)
  :serial t
  :components
  ((:file "5amfix")
   (:file "main-test-suite")
   (:file "package")
   (:file "nisp-asdf")
   (:module "random"
            :components
            ((:file "random")))
   (:file "safe")))

(defsystem :nispbot
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "irc bot"
  :weakly-depends-on (:fiveam)
  :depends-on (:cl-ppcre
               :cl-irc
               :nisp)
  :serial t
  :components
  ((:file "nisp-introspect")
   (:file "config-dist")
   (:file "config")
   (:file "safe-scope")
   (:file "nispbot")))