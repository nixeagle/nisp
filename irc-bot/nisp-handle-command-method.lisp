;;; Sticking this in its own file to make sbcl happy
(in-package :nisp-core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass nisp-command-network-tree-generic-function
      (network-tree-generic-function) ()
    (:metaclass closer-mop:funcallable-standard-class)
    (:default-initargs :method-class (find-class 'handle-nisp-command-method)
      #+ccl :closer-patch #+ccl t))

  (defclass handle-command-method (network-tree-method)
    ((plugin :initarg :plugin :initform nil)
     (call-count :initform 0 :type fixnum
                 :reader handle-command-method-call-count))))

(defmethod print-object ((obj handle-command-method) stream)
  (print-unreadable-object (obj stream)
    (format stream "~{~(~A~)~^-~}" (network-tree-node-keys (eql-specializer-object  (car (method-specializers obj)))))))

(defgeneric route-command (source from content to sink))

(defgeneric handle-command (tree source from address identity
                                      action content)
  (:generic-function-class nisp-command-network-tree-generic-function)
  (:method-class handle-command-method)
 #+ () (:method-combination nisp-standard-method-combination:nisp-standard))

(defgeneric (setf handle-command-method-call-count)
    (value object))

(defmethod (setf handle-command-method-call-count)
    ((value integer) (object handle-command-method))
  (if (= (1+ (slot-value object 'call-count)) value)
      (setf (slot-value object 'call-count) value)
      (error "Doing anything other then incrementing the call-count vie
      setf is not permitted.")))
(defvar *debug* nil
  "Set to t to enable debug mode, this mode means no outgoing messages are
  sent which allows testing locally without spamming various services
  needlessly.")

(defmethod make-method-lambda
    ((generic-function nisp-command-network-tree-generic-function)
     (method handle-command-method)
     expression environment)
  (multiple-value-bind (body lambda-args declarations)
      (parse-method-lambda-expression-body expression)
    (let ((args (gensym "ARGS"))
          (next-methods (gensym "NEXT-METHODS"))
          (this-method (gensym "THIS-METHOD"))
          #-sbcl (a (gensym "A")))
      (let ((lamb
             `(lambda (,args ,next-methods ,this-method
                       #-sbcl &rest #-sbcl ,a)
                (declare (ignorable ,this-method))
                (,(call-next-method generic-function method
                                    `(lambda ,lambda-args
                                       ,@declarations
                                       ,(destructuring-bind
                                         (tree source from address identity
                                               action content)
                                         lambda-args
                                         (declare (ignore tree from identity content))
                                         `(labels ((reply (&rest message)
                                                     (if *debug*
                                                         `(send ,,action ,,source ,,address ,@message)
                                                         (apply #'send ,action ,source ,address message))))
                                            (declare (ignorable (function reply)))
                                            ,@body)))
                                    environment)
                  ,args ,next-methods #-sbcl ,a))))

        lamb) )))

(defmethod compute-effective-method
    ((generic-function network-tree-generic-function)
     (method-combination t) methods)
  `(call-method ,(car methods) ,(cdr methods) ,(car methods)))

(defmacro define-simple-command (name &body body)
  "Defines the command NAME which runs the forms in BODY.

All commands do one of 3 things. Reply to the current context, pass
control to a subcommand, or do nothing.

Inside simple-command forms, there are two 3 important local functions
that can be called.

  - (NEXT-NODE) passes control from the current command to a subcommand
    that is defined seperately.

  - (REMAINING-PARAMETERS) returns a string with any remaining arguments
    to the command or sub-command

  - (REPLY FORMAT-STRING &rest ARGUMENTS) Replies using the same semantics
    as (FORMAT nil FORMAT-STRING ARG1 ARG2 ARG3...), but the formatted
    string is automatically replied to the correct location."
  `(defmethod handle-command
       ((tree (eql #-sbcl(network-tree::intern-network-tree-node
                     ,(substitute #\Space #\- (symbol-name name)))
                   #+sbcl ,(substitute #\Space #\- (symbol-name name))))
        (source abstract-data-source)
        (user abstract-user)
        (address abstract-target)
        (identity abstract-identity)
        (action abstract-action)
        (content abstract-text-message-content))
     ,@body))




;;; END
