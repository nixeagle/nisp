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