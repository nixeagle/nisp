(in-package :nisp-safe-system)

(defpackage #:nisp-empty-package
  (:use :cl :lift :nisp-util)
  (:export #:with-empty-package
           #:make-empty-package
           #:with-package
           #:gen-empty-package
           ))

(defpackage #:nisp-safe
  (:use :common-lisp :lift
        :nisp-empty-package
        :nisp-util)
  (:export #:with-safe-package
           #:with-safe-readtable
           ))