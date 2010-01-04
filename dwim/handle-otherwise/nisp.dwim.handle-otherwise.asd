(in-package :cl-user)
(defpackage #:illusion-system
  (:use :cl :asdf))
(in-package :illusion-system)

(defsystem :nisp.dwim.handle-otherwise
  :license "public domain"
  :depends-on (:hu.dwim.def)
  :components ((:file "handle-otherwise")))