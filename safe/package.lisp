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

(defmacro define-export-system (name from &rest symbol-list)
  `(defpackage ,name
     (:use)
     (:import-from ,from ,@(values symbol-list))
     (:export ,@(values symbol-list))))

(define-export-system :nisp-safe-introspect :nisp-introspect
  #:function-lambda-list)

;;; This is NOT safe on its own currently, however testing timeouts in
;;; the irc portion. Eventually that needs to move here.
(define-export-system :nisp-unsafe-iteration :cl
  #:loop)