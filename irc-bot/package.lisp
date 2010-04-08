(defpackage #:nisp.i
  (:use :cl :iterate :split-sequence :nisp.util.packages :alexandria :eos
         :closer-mop :nisp.network-tree :anaphora)
  (:import-from :cl-irc :user #:nickname #:username #:realname)

  ;; This whole shadowing import block is taken from
  ;; closer-mop-packages.lisp in order to correctly import the
  ;; closer-mop package.
  (:shadowing-import-from  :closer-mop
                             #:built-in-class
                             #:class
                             #:direct-slot-definition
                             #:effective-slot-definition
                             #:eql-specializer
                             #:forward-referenced-class
                             #:funcallable-standard-class
                             #:funcallable-standard-object
                             #:generic-function
                             #:metaobject
                             #:method
                             #:method-combination
                             #:slot-definition
                             #:standard-accessor-method
                             #:standard-class
                             #:standard-generic-function
                             #:standard-direct-slot-definition
                             #:standard-effective-slot-definition
                             #:standard-method
                             #:standard-object
                             #:standard-reader-method
                             #:standard-slot-definition
                             #:standard-writer-method

                             #:defclass
                             #:defgeneric
                             #:define-method-combination
                             #:defmethod

                             #:classp
                             #:ensure-finalized
                             #:ensure-method
                             #:fix-slot-initargs
                             #:required-args
                             #:subclassp

                             #:accessor-method-slot-definition
                             #:add-dependent
                             #:add-direct-method
                             #:add-direct-subclass
                             #:class-default-initargs
                             #:class-direct-default-initargs
                             #:class-direct-slots
                             #:class-direct-subclasses
                             #:class-direct-superclasses
                             #:class-finalized-p
                             #:class-precedence-list
                             #:class-prototype
                             #:class-slots
                             #:compute-applicable-methods-using-classes
                             #:compute-class-precedence-list
                             #:compute-default-initargs
                             #:compute-discriminating-function
                             #:compute-effective-method
                             #:compute-effective-method-function
                             #:compute-effective-slot-definition
                             #:compute-slots
                             #:direct-slot-definition-class
                             #:effective-slot-definition-class
                             #:ensure-class
                             #:ensure-class-using-class
                             #:ensure-generic-function
                             #:ensure-generic-function-using-class
                             #:eql-specializer-object
                             #:extract-lambda-list
                             #:extract-specializer-names
                             #:finalize-inheritance
                             #:find-method-combination
                             #:funcallable-standard-instance-access
                             #:generic-function-argument-precedence-order
                             #:generic-function-declarations
                             #:generic-function-lambda-list
                             #:generic-function-method-class
                             #:generic-function-method-combination
                             #:generic-function-methods
                             #:generic-function-name
                             #:intern-eql-specializer
                             #:make-method-lambda
                             #:map-dependents
                             #:method-function
                             #:method-generic-function
                             #:method-lambda-list
                             #:method-specializers
                             #:reader-method-class
                             #:remove-dependent
                             #:remove-direct-method
                             #:remove-direct-subclass
                             #:set-funcallable-instance-function
                             #:slot-boundp-using-class
                             #:slot-definition-allocation
                             #:slot-definition-initargs
                             #:slot-definition-initform
                             #:slot-definition-initfunction
                             #:slot-definition-location
                             #:slot-definition-name
                             #:slot-definition-readers
                             #:slot-definition-writers
                             #:slot-definition-type
                             #:slot-makunbound-using-class
                             #:slot-value-using-class
                             #:specializer-direct-generic-functions
                             #:specializer-direct-methods
                             #:standard-instance-access
                             #:subtypep
                             #:typep
                             #:update-dependent
                             #:validate-superclass
                             #:writer-method-class
                             #:warn-on-defmethod-without-generic-function
                             ))

(defpackage #:nisp.i-zeta
  (:use :cl :eos :iterate :alexandria)
  (:shadowing-import-from  :closer-mop
                             #:built-in-class
                             #:class
                             #:direct-slot-definition
                             #:effective-slot-definition
                             #:eql-specializer
                             #:forward-referenced-class
                             #:funcallable-standard-class
                             #:funcallable-standard-object
                             #:generic-function
                             #:metaobject
                             #:method
                             #:method-combination
                             #:slot-definition
                             #:specializer
                             #:standard-accessor-method
                             #:standard-class
                             #:standard-generic-function
                             #:standard-direct-slot-definition
                             #:standard-effective-slot-definition
                             #:standard-method
                             #:standard-object
                             #:standard-reader-method
                             #:standard-slot-definition
                             #:standard-writer-method

                             #:defclass
                             #:defgeneric
                             #:define-method-combination
                             #:defmethod

                             #:classp
                             #:ensure-finalized
                             #:ensure-method
                             #:fix-slot-initargs
                             #:required-args
                             #:subclassp

                             #:accessor-method-slot-definition
                             #:add-dependent
                             #:add-direct-method
                             #:add-direct-subclass
                             #:class-default-initargs
                             #:class-direct-default-initargs
                             #:class-direct-slots
                             #:class-direct-subclasses
                             #:class-direct-superclasses
                             #:class-finalized-p
                             #:class-precedence-list
                             #:class-prototype
                             #:class-slots
                             #:compute-applicable-methods-using-classes
                             #:compute-class-precedence-list
                             #:compute-default-initargs
                             #:compute-discriminating-function
                             #:compute-effective-method
                             #:compute-effective-method-function
                             #:compute-effective-slot-definition
                             #:compute-slots
                             #:direct-slot-definition-class
                             #:effective-slot-definition-class
                             #:ensure-class
                             #:ensure-class-using-class
                             #:ensure-generic-function
                             #:ensure-generic-function-using-class
                             #:eql-specializer-object
                             #:extract-lambda-list
                             #:extract-specializer-names
                             #:finalize-inheritance
                             #:find-method-combination
                             #:funcallable-standard-instance-access
                             #:generic-function-argument-precedence-order
                             #:generic-function-declarations
                             #:generic-function-lambda-list
                             #:generic-function-method-class
                             #:generic-function-method-combination
                             #:generic-function-methods
                             #:generic-function-name
                             #:intern-eql-specializer
                             #:make-method-lambda
                             #:map-dependents
                             #:method-function
                             #:method-generic-function
                             #:method-lambda-list
                             #:method-specializers
                             #:reader-method-class
                             #:remove-dependent
                             #:remove-direct-method
                             #:remove-direct-subclass
                             #:set-funcallable-instance-function
                             #:slot-boundp-using-class
                             #:slot-definition-allocation
                             #:slot-definition-initargs
                             #:slot-definition-initform
                             #:slot-definition-initfunction
                             #:slot-definition-location
                             #:slot-definition-name
                             #:slot-definition-readers
                             #:slot-definition-writers
                             #:slot-definition-type
                             #:slot-makunbound-using-class
                             #:slot-value-using-class
                             #:specializer-direct-generic-functions
                             #:specializer-direct-methods
                             #:standard-instance-access
                             #:subtypep
                             #:typep
                             #:update-dependent
                             #:validate-superclass
                             #:writer-method-class
                             #:warn-on-defmethod-without-generic-function
                             ))
(in-package :nisp.i)

;;; DEBUG KLUDGE for now

(defvar *format-and-send-to-irc-function* nil
  "Temporary hack for sending some debug info to an irc channel as a sideband.")

(defun d/i (formatspec &rest args)
  (funcall *format-and-send-to-irc-function*
         (substitute #\Space #\Newline (apply #'format nil formatspec args))))