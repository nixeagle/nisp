;;; Sticking this in its own file to make sbcl happy
(in-package :nisp.i)
(defclass nisp-command-network-tree-generic-function
    (network-tree-generic-function) ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'handle-nisp-command-method)
    #+ccl :closer-patch #+ccl t))

(defclass handle-nisp-command-method (network-tree-method)
  ((call-count :initform 0 :type non-negative-fixnum
          :reader handle-nisp-command-method-call-count)))
(defgeneric (setf handle-nisp-command-method-call-count)
    (value object))

(defmethod (setf handle-nisp-command-method-call-count)
    ((value integer) (object handle-nisp-command-method))
  (if (= (1+ (slot-value object 'call-count)) value)
      (setf (slot-value object 'call-count) value)
      (error "Doing anything other then incrementing the call-count vie
      setf is not permitted.")))
(defvar *debug* nil
  "Set to t to enable debug mode, this mode means no outgoing messages are
  sent which allows testing locally without spamming various services
  needlessly.")
(defvar *trace-method-lambda* nil
  "Poorman's tracing facility for poking around `make-method-lambda'
  below.")
(defmethod make-method-lambda
    ((generic-function nisp-command-network-tree-generic-function)
     (method handle-nisp-command-method)
     expression environment)
  (multiple-value-bind (body lambda-args declarations)
      (parse-method-lambda-expression-body expression)
    (with-gensyms (args next-methods this-method #-sbcl a)
      (let ((lamb
             `(lambda (,args ,next-methods ,this-method
                       #-sbcl &rest #-sbcl ,a)
                (declare (ignorable ,this-method))
                (when *trace-method-lambda*
                  (print ,args))
                (,(call-next-method generic-function method
                                    `(lambda ,lambda-args
                                       ,@declarations
                                       ,(destructuring-bind
                                         (tree source from address identity
                                               action content)
                                         lambda-args
                                         (declare (ignore tree from identity content))
                                         `(incf (handle-nisp-command-method-call-count
                                                 ,this-method))
                                         `(labels ((reply (message)
                                                     (if *debug*
                                                         `(send ,,action ,,source ,,address ,message)
                                                         (send ,action ,source ,address message))))
                                            (declare (ignorable (function reply)))
                                            ,@body)))
                                    environment)
                  ,args ,next-methods #-sbcl ,a))))

        lamb))))

(defmethod compute-effective-method
    ((generic-function network-tree-generic-function)
     (method-combination t) methods)
  `(call-method ,(car methods) ,(cdr methods) ,(car methods)))

;;; END
