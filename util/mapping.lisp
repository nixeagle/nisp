;;; Various helpers for doing things on a list of items.
(defpackage #:nisp.util-mapping
  (:use :cl :alexandria :iterate :eos :with-fbound)
  (:export :mapcar-if)
  (:nicknames :util-mapping))
(in-package :nisp.util-mapping)
(defun mapcar-if (predicate function list)
  "Map over LIST applying FUNCTION each time PREDICATE is non nil."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (iter (for item :in list)
        (collect (if (funcall predicate item)
                     (funcall function)
                     item))))