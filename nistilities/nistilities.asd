(in-package :cl-user)
(defpackage #:nistilities-system
  (:use :cl :asdf))
(in-package #:nistilities-system)

(defsystem :nistilities
  :version "v0.0.1"
  :author "James S <dev@nixeagle.org>"
  :maintainer "James S <dev@nixeagle.org>"
  :license "GPLv2 or later"
  :description "Helpful utilities"
  :depends-on (:lift)
  :serial t
  :components
  ((:file "package")
   (:file "defconstant")
   (:file "util")))