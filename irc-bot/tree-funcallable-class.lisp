(in-package :nisp.i)

;;;{{{ Tree classes
(defclass tree-generic-function (standard-generic-function)
  ((top-level-tree :initform (make-hash-table :test 'eq :weakness :value)))
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class (find-class 'tree-method)))

(defclass tree-method (standard-method)
  ())

(defclass tree-specializer (eql-specializer)
  ((object :reader tree-specializer-object
           :reader class-name
           :initarg :object)
   (direct-methods :initform nil
                   :reader specializer-direct-methods)
   (direct-nodes :initform (make-hash-table :test 'eq :weakness :value)
                 :reader tree-specializer-direct-nodes)))

(defmethod add-direct-method ((specializer tree-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))

(defmethod remove-direct-method ((specializer tree-specializer) method)
  (deletef (slot-value specializer 'direct-methods) method))
;;;}}}

;;;{{{ Ensure tree symbol
(defparameter +tree-symbols-package+ :nisp.i.command-argument-symbols
  "Package that all tree symbols should get interned into.")

(defgeneric ensure-tree-symbol (symbol)
  (:documentation "Make sure SYMBOL exists in `+tree-symbols-package+'."))
(defmethod ensure-tree-symbol ((symbol symbol))
  "Make SYMBOL a list and recall."
  (ensure-tree-symbol (list symbol)))
(defmethod ensure-tree-symbol ((symbols cons))
  "SYMBOLS get interned into `+tree-symbols-package+'."
  (mapcar (lambda (symbol)
            (ensure-symbol symbol +tree-symbols-package+))
          symbols))
(defmethod ensure-tree-symbol ((symbols string))
  "Split by spaces, then intern SYMBOLS as normal."
  (ensure-tree-symbol
   (split-sequence #\Space (string-upcase symbols) :remove-empty-subseqs t)))
;;;}}}



(defmethod compute-applicable-methods-using-classes
    ((generic-function tree-generic-function) classes)
  "No cache permitted right now."
  (values classes nil))

(defmethod compute-applicable-methods
    ((generic-function tree-generic-function) args)

  ;; Nothing special yet
  (call-next-method))

(defmethod compute-effective-method
    ((generic-function tree-generic-function)
     method-combination applicable-methods)
  ;; Nothing special yet
  (call-next-method))

(defmethod compute-discriminating-function
    ((generic-function tree-generic-function))
  (let ((it (call-next-method)))
    (describe it)
    it))


(defmethod make-method-lambda
    ((generic-function tree-generic-function) method expression environment)
  (declare (ignore environment))
  (call-next-method))

(defmethod make-load-form ((self tree-specializer) &optional env)
  (declare (ignore env))  `(intern-command-specializer ',(command-specializer-object self)))

#+ ()
(defun maybe-intern-command-specializer (input)
  (if (and (consp input) (eq (car input) 'command))
      (intern-command-specializer (cdr input))
      input))
#+ ()
(defun transform-method-specializers (input)
  (mapcar #'maybe-intern-command-specializer input))
;;; Now we need to make a specializer class. Most of this is from sbcl's
;;; boot.lisp `real-make-method-specializers-form'.
#+ ()
(defmethod sb-pcl:make-method-specializers-form
    ((generic-function command-generic-function)
     method specializer-names environment)
  (let ((names (transform-method-specializers specializer-names)))
    (call-next-method generic-function method names environment)))