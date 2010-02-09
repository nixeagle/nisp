(in-package :cl-user)
(defpackage #:nisp.fbi-system
  (:use :cl :asdf))
(in-package :nisp.fbi-system)

(defsystem :nisp.user
  :version "0.1.0"
  :author "James S <i@nixeagle.org>"
  :maintainer "James S <i@nixeagle.org>"
  :license "GPLv3 or later"
  :depends-on (:iterate
                :md5
                :cxml-stp
                :hunchentoot
                :cl-irc
                :cl-who
                :closure-html
                :drakma 
                :usocket
                :closure-html
                :cl-html-parse
                :djula
                :nisp.util
                :clithub
                :nisp.fbi)
  :serial t
  :components
  ((:file "user")))