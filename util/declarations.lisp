(defpackage #:nisp.util.declarations
  (:use :cl :eos :with-fbound :iterate :alexandria)
  (:nicknames :util.declarations)
  (:documentation "Portably look at compiler declaration state.")
  (:export :get-safety-setting))
(in-package :nisp.util.declarations)

(declaim (ftype (function () (values (integer 0 3) &optional))
                get-safety-setting))
(defun get-safety-setting ()
  "Returns current compiler safey setting.

Value is normally set with `declaim' `proclaim' or `declare'."
  (or #+sbcl (cdr (assoc 'safety sb-c::*policy*))
      (error "Unknown implementation, can't get safety settings.")))