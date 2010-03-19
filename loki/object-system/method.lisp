(in-package :loki-object-system)

(defstruct (method-object
             (:conc-name method-)
             (:include data-mixin)
             (:constructor)
             (:constructor create-method-object
                           (&key direct-mimics direct-imitators
                                 direct-cells lambda-list forms))
             (:print-object
              (lambda (obj *standard-output*)
                (if *print-readably*
                    (format *standard-output*
                            "#S(~S :direct-mimics ~S :direct-cells ~S :lambda-list ~S :forms ~S)"
                            (type-of obj) (direct-mimics obj)
                            (direct-cells obj) (method-lambda-list obj)
                            (method-forms obj))
                    (call-next-method)))))
  (function nil :type (or function null))
  (lambda-list '() :type list)
  (forms '() :type list))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-method-lambda-form (this-method arguments expression)
    (multiple-value-bind (forms declarations docstring)
        (parse-body expression  :documentation t :whole t)
      (progn
        `(lambda (message receiver context ,@arguments)
           ,docstring
           (declare (type (or null object) context message receiver))
           ,@declarations
           (let ((this-method ,this-method)
                 (|self| receiver)
                 (@ receiver)
                 (|currentMessage| message)
                 (context context)
                 (|surroundingContext| *surrounding-context*))
             (declare (ignorable this-method |self| @ |currentMessage|
                                 context |surroundingContext|))
             ,@forms))))))

(defmacro make-method-lambda (this-method arguments expression)
  `#',(make-method-lambda-form this-method arguments expression))

(defmethod make-load-form ((self method-object) &optional env)
  (declare (ignore env))
  (values
   `(make-method-object :direct-mimics ',(direct-mimics self)
                        :direct-cells ',(direct-cells self)
                        :lambda-list ',(method-lambda-list self)
                        :forms ',(method-forms self))
   `(setf (method-function ',self)
          (make-method-lambda ,self
                              ,(method-lambda-list self)
                              ,(method-forms self)))))

(defun call-method (object &rest args)
  "Apply ARGS to OBJECT's `method-function'."
  (declare (type method-object object))
  (apply (method-function object) args))

(defmacro make-method (lambda-list &body body)
  (create-method-object :lambda-list lambda-list
                        :forms body))