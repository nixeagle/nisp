(defpackage #:nisp.util.palindrome
  (:use :cl)
  (:export #:palindromep :palindrome))
(in-package :nisp.util.palindrome)

(defun palindromep (object)
  "True when OBJECT is the same printed reversed."
  (string= (princ-to-string object)
           (reverse (princ-to-string object))))

(deftype palindrome ()
  '(satisfies #'palindromep))