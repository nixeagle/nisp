
;;This is not pretty at all conceptually, but we need nisp here for the time being otherwise things in package won't see all-tests.
(defpackage #:nisp
  (:use :cl :eos :with-fbound))

(in-package :nisp)
(define-new-suite :nisp-eos-root)