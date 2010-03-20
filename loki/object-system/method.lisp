(in-package :loki-object-system)

(defstruct (method-object
             (:conc-name method-)
             (:include data-mixin)
             (:constructor)
             (:print-object
              (lambda (obj *standard-output*)
                (if *print-readably*
                    (format *standard-output*
                            "#S(~S :direct-mimics ~S :direct-cells ~S :lambda-list ~S :forms ~S :docstring ~S :declarations ~S)"
                            (type-of obj) (direct-mimics obj)
                            (direct-cells obj) (method-lambda-list obj)
                            (method-forms obj)
                            (docstring obj)
                            (method-declarations obj))
                    (call-next-method)))))
  (declarations '() :type list)
  (function nil :type (or null function)) ;Slot is temp nil at load/compile
  (lambda-list '() :type list)
  (forms '() :type list))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-method-lambda (this-method declarations docstring
                                  arguments forms)
    `(lambda (message receiver context ,@arguments)
       ,docstring
       (declare (type object context message receiver))
       ,@declarations
       (let ((this-method ,this-method)
             (|self| (or receiver *ground*))
             (@ (or receiver *ground*))
             (|currentMessage| message)
             (context (or context *context*))
             (|surroundingContext| *surrounding-context*))
         (declare (ignorable this-method |self| @ |currentMessage|
                             context |surroundingContext|))
         ,@forms))))

#+ ()
(defmacro make-method-lambda (this-method arguments expression)
  `#',(make-method-lambda-form this-method arguments expression))

(defmethod make-load-form ((self method-object) &optional env)
  (declare (ignore env))
  (values
   `(make-method-object :direct-mimics ',(direct-mimics self)
                        :direct-cells ',(direct-cells self)
                        :lambda-list ',(method-lambda-list self)
                        :docstring ',(docstring self)
                        :declarations ',(method-declarations self)
                        :forms ',(method-forms self))
   `(setf (method-function ',self)
          (make-method-lambda ,self
                              ,(method-declarations self)
                              ,(docstring self)
                              ,(method-lambda-list self)
                              ,(method-forms self)))))

(defun call-method (object receiver &rest args)
  "Apply ARGS to OBJECT's `method-function'."
  (declare (type method-object object))
  (apply (method-function object) (make-object)
         receiver (make-object) args))

(defun call (receiver method-name &rest args)
  (declare (type object receiver))
  (apply (method-function (cell receiver method-name))
         (make-object) receiver (make-object) args))

(defmacro make-method (lambda-list &body body)
  (multiple-value-bind (forms declarations docstring)
      (parse-body body  :documentation t :whole t)
    (with-gensyms (this-method)
      `(let ((,this-method
              ,(make-method-object #+ :direct-mimics (list *origin*)
                                   :lambda-list lambda-list
                                   :forms forms
                                   :docstring docstring
                                   :declarations declarations)))
         (setf (method-function ,this-method)
               ,(make-method-lambda-form this-method declarations docstring
                                         lambda-list forms))
         ,this-method))))