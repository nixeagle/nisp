(defpackage #:nisp-empty-package
  (:use :cl #+5am :5am :lift :nisp-util)
  (:export #:with-empty-package
           #:make-empty-package
           #:with-package
           ))

(defpackage #:nisp-safe
  (:use :common-lisp #+5am :5am :lift
        :nisp-empty-package
        :nisp-util))

(in-package :nisp-safe)

