(in-package :nisp-safe-system)






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

;;; things required to get some minimal support going
(define-export-system :nisp-safe-misc :cl
  #:list)

(defpackage #:nisp-empty-package
  (:use :cl :lift :nistilities)
  (:export #:with-empty-package
           #:make-empty-package
           #:with-package
           #:gen-empty-package
           ))

(defpackage #:nisp-safe
  (:use :common-lisp
        :lift
        :metatilities
        :nisp-empty-package
        :nistilities)
  (:export #:with-safe-package
           #:with-safe-readtable
           :safe-set
           #:safe-read
           #:safe-select
           #:make-safe-set
           ))



;;; Used as subpackages for sandboxed code
(defpackage #:safe-testing!
  (:use)
  (:shadowing-import-from :cl #:loop #:mapc #:mapcar #:list :t :nil)
  (:shadowing-import-from :nistilities
                          #:range)
  (:export
           #:loop
           #:mapc
           #:mapcar
a           #:list
           #:range
           :t
           :nil))

(defpackage #:safe-external
  (:use)
  (:export help test-results))

(defpackage #:safe-closure
  (:use)
  (:export #:reset :nisp-test))