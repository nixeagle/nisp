(in-package :cl-user)
(defpackage #:nisp.alpha
  (:use :common-lisp :iterate)
  (:nicknames :alpha :a))
(in-package :nisp.alpha)

(defgeneric object->defstruct-description (object)
  (:documentation "Convert OBJECT to defstruct-description."))
(defmethod object->defstruct-description ((object sb-kernel:defstruct-description))
  "Return identity of OBJECT."
  object)
(defmethod object->defstruct-description ((symbol symbol))
  "Find structure description for SYMBOL.

This implies that SYMBOL actually refers to a structure."
  (declare (type class-symbol symbol))
  (sb-pcl::find-defstruct-description symbol))
(defmethod object->defstruct-description ((structure-instance structure-object))
  "Find structure description for STRUCTURE-INSTANCE."
  (object->defstruct-description (class-of structure-instance)))
(defmethod object->defstruct-description ((class structure-class))
  "Find structure description for CLASS"
  (object->defstruct-description (class-name class)))

(defun dd-name-equal-p (slot name-symbol)
  "Return t if the name of SLOT is eql to NAME-SYMBOL."
  (eql (sb-kernel:dsd-name slot) name-symbol))

;;; make work with arbitrary designator?
(defun structure-slots (structure-designator)
  "List all slots of STRUCTURE-DESIGNATOR."
  (declare (type structure-designator structure-designator))
  (sb-vm::dd-slots (object->defstruct-description structure-designator)))

(defun structure-slot-by-name (structure-designator slot-name)
  "Return description of SLOT-NAME in STRUCTURE-DESIGNATOR."
  (declare (type structure-designator structure-designator)
           (type class-symbol slot-name))
  (iter (for slot :in (structure-slots structure-designator))
        (finding (the sb-kernel:defstruct-slot-description slot)
                 :such-that (dd-name-equal-p slot slot-name))))

;;;; Testing structure operations
(defstruct 4-slot-structure a b c d)

(defparameter *4slot* (make-4-slot-structure :a "hi" :b 23 :d "hello!"))
