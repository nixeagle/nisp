(in-package :cl-user)
(defpackage #:nisp.google-system
  (:use :cl :asdf))
(in-package :nisp.google-system)


(defsystem :nisp.google
  :version "0.1.0"
  :author "James S <i@nixeagle.org>"
  :maintainer "James S <i@nixeagle.org>"
  :license "GPLv3 or later"
  :depends-on (:iterate
                :cxml-stp
                :closure-html
                :drakma 
                :nisp.util)
  :serial t
  :components
  ((:file "google")))