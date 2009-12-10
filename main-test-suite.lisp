
;;This is not pretty at all conceptually, but we need nisp here for the time being otherwise things in package won't see all-tests.
(defpackage #:nisp
  (:use :cl :lift))

(in-package :nisp)
(deftestsuite root-suite () ())

(5am:def-suite all-tests
    :description "Top level test suite")