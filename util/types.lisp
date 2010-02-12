(in-package :nisp.util-system)
(defpackage #:nisp.util-types
  (:use :common-lisp :iterate)
  (:export #:type-specifier-p
           type-specifier
           #:class-symbol-p
           class-symbol
           positive-fixnum
           structure-designator))
(in-package :nisp.util-types)

(deftype positive-fixnum ()
  "A fixnum"
  `(integer 1 ,most-positive-fixnum))

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

(defun class-symbol-p (symbol)
  "Return t if SYMBOL identifies a class."
  (if (find-class symbol nil) t))

(deftype class-symbol ()
  "Symbol that identifies a class."
  '(and symbol (satisfies class-symbol-p)))

(deftype structure-designator ()
  '(or symbol structure-class structure-object))