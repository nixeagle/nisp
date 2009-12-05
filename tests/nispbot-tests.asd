(in-package :cl-user)

(defpackage #:nispbot-system
  (:use :cl :asdf))

(in-package #:nispbot-system)

(defsystem :nispbot-tests
  :components ((:file "nispbot-tests"))
  :depends-on (:FiveAM
               :nispbot))