;; system definition file
(in-package :cl-user)
(defpackage #:nispbot-system
  (:use :cl :asdf))
(in-package #:nispbot-system)
(defpackage #:nisp-safe-system
  (:use :cl :asdf))
(defsystem :nisp
  :version "0.0.3"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "Nixeagle's random lisp experiments"
  :depends-on (:cl-ppcre
               :lift
               :cl-irc
               :trivial-timeout
               :trivial-shell)
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
             (:file "safe")))
   (:file "config-dist")
   (:file "config")
   (:file "nispbot")
   (:file "nisp-dev-helper")))

;; (defsystem :nispbot
;;   :author "James S <dev@nixeagle.org>"
;;   :maintainer "James S <dev@nixeagle.org>"
;;   :license "GPLv2 or later"
;;   :description "irc bot"
;;   :depends-on (:lift
;;                :cl-ppcre
               
;;                :trivial-timeout
;;                :nisp)


;;   :serial t
;;   :components
;;   )

;; (defsystem #:nisp-dev-helper
;;   :depends-on (:lift :nisp :nispbot :trivial-shell)
;;   :components ((:file "nisp-dev-helper")))