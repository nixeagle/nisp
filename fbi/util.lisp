(defpackage #:nisp.fbi.util
  (:use :cl :iterate :nisp.util-types)
  (:export #:make-keyword))

(in-package :nisp.fbi.util)

(defun make-keyword (string)
  "Convert STRING to a keyword (uppercased)."
  (declare (type string string))
  (intern (string-upcase string) :keyword))