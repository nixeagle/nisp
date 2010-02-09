(with-fbound:define-new-suite :nisp-eos-root)

(defpackage #:nisp.mop-simple
  (:use :cl :eos :with-fbound))

(defpackage #:nisp.mop
  (:use :cl :nisp.mop-simple :eos :with-fbound))