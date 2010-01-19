(in-package :cl-user)
(defpackage #:nisp.alpha
  (:use :common-lisp :iterate)
  (:nicknames :alpha :a))
(in-package :nisp.alpha)

;; Idea for (documentation symbol 'type) taken from slime.lisp
(defun type-specifier-p (symbol)
  "True if SYMBOL is a type."
  (or (documentation symbol 'type)
      #+:sbcl
      (multiple-value-bind (arglist exists)
          (sb-introspect:deftype-lambda-list symbol)
        (values exists arglist))))

(deftype type-specifier ()
  "Type specifier that can be used with TYPEP"
  '(satisfies type-specifier-p))

(defun export-some-internals (package-name)
  (iter (for (symbol state) :in-packages package-name :having-access (:internal))
        (when (or (type-specifier-p symbol)
                  (fboundp symbol))
          (export symbol package-name)
          (collect symbol))))

(deftype structure-designator ()
  '(or symbol structure-class structure-object))

(defgeneric object->defstruct-description (object)
  (:documentation "Convert OBJECT to defstruct-description."))
(defmethod object->defstruct-description ((object sb-kernel:defstruct-description))
  "Return identity of OBJECT."
  object)
(defmethod object->defstruct-description ((symbol symbol))
  "Find structure description for SYMBOL.

This implies that SYMBOL actually refers to a structure."
  (sb-pcl::find-defstruct-description symbol))
(defmethod object->defstruct-description ((structure-instance structure-object))
  "Find structure description for STRUCTURE-INSTANCE."
  (object->defstruct-description (class-of structure-instance)))
(defmethod object->defstruct-description ((class structure-class))
  "Find structure description for CLASS"
  (object->defstruct-description (class-name class)))

#+sbcl
(defun dd-name-equal-p (slot name-symbol)
  "Return t if the name of SLOT is eql to NAME-SYMBOL."
  (eql (sb-kernel:dsd-name slot) name-symbol))

;;; make work with arbitrary designator?
#+sbcl
(defun structure-slots (structure-designator)
  "List all slots of STRUCTURE-DESIGNATOR."
  (declare (type structure-designator structure-designator))
  (sb-vm::dd-slots (object->defstruct-description structure-designator)))

;;;; Testing structure operations
(defstruct 4-slot-structure a b c d)

(defparameter *4slot* (make-4-slot-structure :a "hi" :b 23 :d "hello!"))

