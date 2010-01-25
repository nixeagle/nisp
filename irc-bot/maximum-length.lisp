(defpackage #:nisp.clos.maximum-length
  (:use :cl :nisp.util-types)
  (:export :maximum-length-mixin
           :limited-length-mixin
           :maximum-length
           #:valid-lenth-p))
(in-package :nisp.clos.maximum-length)

(defclass maximum-length-mixin () ()
  (:documentation "Object expected to respond to and setf a maximum-length.

How or where the slot (if there even is one) is stored does not matter."))

(defclass limited-length-mixin (maximum-length-mixin) ()
    (:documentation "Represents objects where it makes sense to call valid-length-p.
This is the set of all objects that decompose to a string."))

(defgeneric maximum-length (object)
  (:method ((object maximum-length-mixin))
    "Subclassing MAXIMUM-LENGTH-MIXIN requires this method to be defined.

Additionally you _must_ return a number."
    (error "Method on mixin class invalid.")))

(defgeneric (setf maximum-length) (length object)
  (:method (length (object maximum-length-mixin))
    (error "Method on mixin class invalid.")))

(defgeneric valid-length-p (object)
  (:method (object)
    (error "Method on mixin class invalid.")))

(defclass maximum-length (maximum-length-mixin)
  ((maximum-length :type positive-fixnum
                   :accessor maximum-length
                   :initarg :maximum-length
                   :initform (error "Maximum length must be specified.")
                   :documentation "Maximum length a sequence may be.")))