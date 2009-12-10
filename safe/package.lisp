(defpackage #:nisp-empty-package
  (:use :cl :lift :nisp-util)
  (:export #:with-empty-package
           #:make-empty-package
           #:with-package
           ))

(defpackage #:nisp-safe
  (:use :common-lisp :lift
        :nisp-empty-package
        :nisp-util)
  (:export #:with-safe-package))

(in-package :nisp-safe)

