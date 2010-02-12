(defpackage #:nisp.mop-simple
  (:use :cl :eos :with-fbound :iterate :alexandria)
  (:export
   ;; Generic function readers
   :generic-function-name :generic-function-method-class
   :generic-function-lambda-list :generic-function-method-combination
   :generic-function-argument-precedence-order
   :generic-function-declarations :generic-function-methods

   ;; Class readers
   :class-direct-slots :class-precedence-list :class-direct-subclasses
   :class-direct-default-initargs :class-slots :class-direct-superclasses
   :class-default-initargs :class-finalized-p :class-prototype
   ))

(defpackage #:nisp.mop
  (:use :cl :nisp.mop-simple :eos :with-fbound)
  (:export :compare-generic-applicable-methods))

(defpackage #:nisp.mop-store
  (:use :cl :eos :with-fbound :alexandria :iterate))