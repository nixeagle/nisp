(in-package :cl-user)
(defpackage #:nisp.fbi-system
  (:use :cl :asdf))
(in-package :nisp.fbi-system)

(defsystem :nisp.fbi
  :version "0.1.0"
  :author "James S <i@nixeagle.org>"
  :maintainer "James S <i@nixeagle.org>"
  :license "GPLv3 or later"
  :depends-on (:iterate
                :alexandria
                :with-fbound
                :usocket
                :nisp.util.usocket
                :nisp.util
                :bordeaux-threads
                :nisp.util.json
                :cl-json)
  :serial t
  :components
  ((:file "util")
   (:file "json-classes")
   (:file "fbi")
   (:file "irc-commands")
   (:file "tests")))