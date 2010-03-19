(in-package :cl-user)

(defpackage #:loki-asd
  (:use :cl :asdf)
  (:export #:+loki-version+))

(in-package :loki-asd)

(defparameter +loki-version+ "0.0.1"
  "String indicating current version of loki.")

(defsystem :loki
  :version #.+loki-version+
  :depends-on (:alexandria
               :split-sequence
               :iterate
               :bordeaux-threads)
  :serial t
  :components
  (#+ () (:module mop
            :serial t
            :components ((:file "packages")
                         (:file "boot")))
   (:module object-system
            :serial t
            :components ((:file "boot")
                         (:file "method")
                         (:file "userland-setup")))
   (:file "main-package")))