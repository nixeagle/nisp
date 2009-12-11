;;; These package and function definitions are *alpha* quality.

(in-package :nisp-safe-system)


;;; These should be fairly safe as there are no side effects,
;;; at least according to the standard.
(defpackage #:safe-arithmetic
  (:use :cl)
  (:export
   ;; basic operators
   #:+ #:- #:* #:/ #:=
   ;; basic incrementers
   #:1+ #:1-
   ;; common denometer stuff
   #:gcd #:lcm
   ;; arithmetic
   #:abs
   ;; trig
   #:sin #:cos #:tan
   #:asin #:acos #:atan
   #:sinh #:cosh #:tanh
   #:asinh #:acosh #:atanh

   ;; complex numbers
   #:conjugate
   #:cis #:sqrt #:issqrt
   #:exp #:expt
   #:log #:signum #:phase pi

   ;;comparision
   #:/=
   #:<
   #:<=
   #:=
   #:>
   #:>=
   #:oddp
   #:evenp
   #:plusp
   #:max
   #:zerop
   #:min
   #:minusp
   ))


;;; These do modifications to variables, eg side effects.
(defpackage #:safe-arithmetic!
  (:use :cl)
  (:export
   ;; basic incrementers
   #:decf
   #:incf))
