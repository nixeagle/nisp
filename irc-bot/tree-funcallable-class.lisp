(in-package :nisp.i)

;;;{{{ Tree classes
(defclass tree-generic-direct-nodes ()
  ((direct-nodes :initform (make-hash-table :test 'eq :weakness :value)
                 :reader tree-generic-direct-nodes)))

(defclass tree-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class (find-class 'tree-method)))


(defclass tree-method (standard-method)
  ())

(defclass network-tree-node (tree-generic-direct-nodes)
  ((object :initarg :object
           :reader network-tree-node-object)))


;;;}}}

(defparameter +tree-symbols-package+ :keyword
  "Package that all tree symbols should get interned into.")

(defun tree-symbol-string-p (string)
  "True if STRING can represent a valid tree-symbol.

A valid tree-symbol is defined as anything that does not contain a space."
  (declare (type string string))
  (not (find #\Space string)))
(deftype tree-symbol-string ()
  '(and string (satisfies tree-symbol-string-p)))


;;;{{{ Ensure tree symbol
(defgeneric ensure-tree-symbol (symbol)
  (:documentation "Make sure SYMBOL exists in `+tree-symbols-package+'."))
(defmethod ensure-tree-symbol (arg)
  "Make a symbol out of ARG by `format-symbol'."
  (format-symbol +tree-symbols-package+ "~A" arg))
(defmethod ensure-tree-symbol ((symbol symbol))
  "Make SYMBOL a list and recall."
  (call-next-method))
(defmethod ensure-tree-symbol ((symbols cons))
  "SYMBOLS get interned into `+tree-symbols-package+'."
  (error "KO"))
(defmethod ensure-tree-symbol ((symbol-as-string string))
  "Split by spaces, then intern SYMBOLS as normal."
  (declare (type tree-symbol-string symbol-as-string))
  (make-keyword (string-upcase symbol-as-string)))

(defgeneric ensure-tree-symbols (symbols)
  (:documentation "Return a list of symbols instead of just one."))

(defmethod ensure-tree-symbols ((args list))
  (mapcar #'ensure-tree-symbol args))
(defmethod ensure-tree-symbols ((args symbol))
  (ensure-tree-symbols (ensure-list args)))
(defmethod ensure-tree-symbols ((symbols-as-string string))
  (ensure-tree-symbols (split-sequence #\Space
                                       symbols-as-string
                                       :remove-empty-subseqs t)))

;;;}}}

(defvar *network-tree-nodes* (make-instance 'tree-generic-direct-nodes))

(defgeneric preprocess-arglist (generic-function args)
  (:documentation
   "Modify ARGS to make them suiable for computing applicable methods."))
(defmethod preprocess-arglist ((generic-function tree-generic-function) args)
  ;; Always return root node, never all the way down to the end of ARGS.
  (cons (gethash (caar args) (tree-generic-direct-nodes *network-tree-nodes*)) (cdr args)))

(defun %intern-tree-specializer (node symbols)
  (declare (type tree-generic-direct-nodes node)
           (type list symbols)
           (optimize (speed 3) (safety 0) (debug 1)))
  (the network-tree-node
    (if symbols
        (%intern-tree-specializer
         (or (gethash (car symbols) (tree-generic-direct-nodes node))
             (setf (gethash (car symbols) (tree-generic-direct-nodes node))
                   (make-instance 'network-tree-node :object (car symbols))))
         (cdr symbols))
        node)))

(defun intern-network-tree-node(symbols)
  "Intern a unique specializer for TREE for SYMBOLS.

SYMBOLS represents a path starting at the root of TREE and going down one
symbol at a time through repeated hash-tables until the last of SYMBOLS is
reached.

SYMBOLS may be a list of symbols, a string of space seperated words that
is translated into a list of symbols."
  (declare (type (or string list keyword) symbols))
  (%intern-tree-specializer *network-tree-nodes* (ensure-tree-symbols symbols)))

(defun maybe-make-tree-specializer-form (specializer-name)
  ;; We don't actually check right now, instead just making the correct
  ;; form.
  ;;
  ;; This _must_ return a form that creates the specializer, never the
  ;; specializer itself. This solves a bug related to SB-PCL::LOAD-DEFMETHOD.
  ;;
  ;; We must sharpsign quote the generic-function name, otherwise the fasl
  ;; cannot be loaded.
  `(eql (intern-network-tree-node ,@(cdr specializer-name))))

;;; Now we need to make a specializer class. Most of this is from sbcl's
;;; boot.lisp `real-make-method-specializers-form'.
(defmethod sb-pcl:make-method-specializers-form
    ((generic-function tree-generic-function)
     method specializer-names environment)
  ;; We always assume a specializer exists in the first element of
  ;; SPECIALIZER-NAMES. We certainly can do better, but this works.
  (call-next-method generic-function method
                    (cons (maybe-make-tree-specializer-form (car specializer-names))
                          (cdr specializer-names))
                    environment))

(defmethod compute-applicable-methods-using-classes
    ((generic-function tree-generic-function) classes)
  "No cache permitted right now."
  (values classes nil))

;;; We don't need this around method right now, we don't even do anything
;;; with the primary!

(defmethod compute-applicable-methods :around
    ((generic-function tree-generic-function) args)
  (call-next-method generic-function (cons (ensure-tree-symbols (car args))
                                           (cdr args))))

(defmethod compute-applicable-methods
    ((generic-function tree-generic-function) args)
  (call-next-method generic-function (preprocess-arglist generic-function
                                                         args)))

(defmethod compute-effective-method
    ((generic-function tree-generic-function)
     method-combination applicable-methods)
  ;; Nothing special yet
  (call-next-method))

(defmethod compute-discriminating-function
    ((generic-function tree-generic-function))
  (let ((it (call-next-method)))
    (lambda (&rest args)
      (apply it (ensure-tree-symbols (car args)) (cdr args)))

#+ ()    it))


(defmethod make-method-lambda
    ((generic-function tree-generic-function) method expression environment)
  (let ((result (call-next-method)))
    `(lambda (args next-methods &rest remaining-args)
#+ ()       (print args)
       (labels ((current-node ()
                (gethash (caar args) (tree-generic-direct-nodes *network-tree-nodes*)))
              (next-node ()
                (list (gethash (cadar args) (tree-generic-direct-nodes (current-node)))
                      (cdr args))))
         (declare (ignorable (function current-node) (function next-node)))
         (,result args next-methods remaining-args)))))

(defmethod make-load-form ((self network-tree-node) &optional env)
  (declare (ignore env))
  (values (intern-network-tree-node (network-tree-node-object self))
          nil))

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