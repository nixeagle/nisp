(defpackage #:nisp-empty-package
  (:use :cl #+5am :5am)
  (:export :with-empty-package))

(defpackage #:nisp-safe
  (:use :common-lisp #+5am :5am
        :nisp-util))

(in-package :nisp-safe)
(5am:def-suite safe-suite
    :in nisp::all-tests)
