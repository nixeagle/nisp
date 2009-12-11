;; system definition file
(in-package :cl-user)
(defpackage #:nispbot-system
  (:use :cl :asdf))
(in-package #:nispbot-system)
(defpackage #:nisp-safe-system
  (:use :cl :asdf))
(defsystem :nisp
  :version "0.0.2"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "Nixeagle's random lisp experiments"
  :depends-on (:cl-ppcre
               :lift)
  :properties ((#:author-email . "dev@nixeagle.org")
               (#:date . "Future")
               ((#:albert #:output-dir) . "albert-docs/")
               ((#:albert #:formats) . ("docbook" "html"))
               ((#:albert #:docbook #:template) . "book")
               ((#:albert #:docbook #:bgcolor) . "white")
               ((#:albert #:docbook #:textcolor) . "black")
               )
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
   (:file "nisp-introspect")
   (:module "safe"
            :components
            ((:file "package")
             (:file "empty-package")
             (:file "arithmetic")
             (:file "safe")))))

(defsystem :nispbot
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "irc bot"
  :depends-on (:lift
               :cl-ppcre
               :cl-irc
               :trivial-timeout
               :nisp)

  :serial t
  :components  ((:file "config-dist")
   (:file "config")
   (:file "nispbot")))