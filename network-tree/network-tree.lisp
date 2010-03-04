(in-package :nisp.network-tree)

(defun first-command-word (command-string &optional (seperater #\Space))
  "Extract the first segment in COMMAND-STRING before SEPERATER.

Second return value is what is left after removing the segment."
  (declare (type string command-string)
           (type character seperater))
  (let ((space-index (position seperater command-string)))
    (declare (type (or null non-negative-fixnum) space-index))
    (if space-index
        (values
         (subseq command-string 0 space-index)
         (subseq command-string (the non-negative-fixnum (1+ space-index))))
        (values command-string ""))))

(defun network-node-string-p (string)
  "True if STRING can represent a valid tree-symbol.

A valid tree-symbol is defined as anything that does not contain a space."
  (declare (type string string))
  (not (find #\Space string)))

(deftype network-node-string ()
  '(and string (satisfies network-node-string-p)))

;;;{{{ Tree classes
(defclass tree-generic-direct-nodes ()
  ((direct-nodes :initform (make-hash-table :test 'eq :weakness :value)
                 :reader tree-generic-direct-nodes)))
(defmethod tree-generic-direct-node ((tree tree-generic-direct-nodes)
                                     (arg string))
  (gethash (ensure-tree-symbol arg) (tree-generic-direct-nodes tree)))
(defmethod tree-generic-direct-node ((tree tree-generic-direct-nodes)
                                     (arg symbol))
  (declare (type keyword arg))
  (gethash arg (tree-generic-direct-nodes tree)))

;;; This is SLOOOOOW, we want to use common-lisp:standard-generic-function
;;; for something like a 10,000 times improvement in speed... which I
;;; don't even know why we are this slow
(defclass abstract-network-tree-generic-function () ()
  (:metaclass closer-mop:funcallable-standard-class))

(defclass slow-network-tree-generic-function
    (standard-generic-function
     abstract-network-tree-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'tree-method)))

(defclass network-tree-generic-function
    (cl:standard-generic-function
     abstract-network-tree-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'tree-method)))


(defclass tree-method (standard-method)
  ())

(defclass network-tree-node (tree-generic-direct-nodes)
  ((object :initarg :object
           :reader network-tree-node-object)))

(defmethod print-object ((obj network-tree-node) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (princ (network-tree-node-object obj) stream)))


;;;}}}

(defparameter +tree-symbols-package+ :keyword
  "Package that all tree symbols should get interned into.")


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
  (declare (type network-node-string symbol-as-string))
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

;;;{{{ Interning network-tree nodes
(defvar *network-tree-nodes* (make-instance 'tree-generic-direct-nodes))

(defun %intern-tree-specializer (node symbols)
  (declare (type tree-generic-direct-nodes node)
           (type list symbols)
           ;; We need tail recursion
           (optimize (speed 3) (safety 0) (debug 1)))
  (the network-tree-node
    (if symbols
        (%intern-tree-specializer
         (or (gethash (car symbols) (tree-generic-direct-nodes node))
             (setf (gethash (car symbols) (tree-generic-direct-nodes node))
                   (make-instance 'network-tree-node :object (car symbols))))
         (cdr symbols))
        node)))

(defun intern-network-tree-node (symbols)
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
;;;}}}

;;; Now we need to make a specializer class. Most of this is from sbcl's
;;; boot.lisp `real-make-method-specializers-form'.
(defmethod sb-pcl:make-method-specializers-form
    ((generic-function network-tree-generic-function)
     method specializer-names environment)
  ;; We always assume a specializer exists in the first element of
  ;; SPECIALIZER-NAMES. We certainly can do better, but this works.
  (call-next-method generic-function method
                    (cons (maybe-make-tree-specializer-form (car specializer-names))
                          (cdr specializer-names))
                    environment))

(defmethod sb-pcl:make-method-specializers-form
    ((generic-function slow-network-tree-generic-function)
     method specializer-names environment)
  ;; We always assume a specializer exists in the first element of
  ;; SPECIALIZER-NAMES. We certainly can do better, but this works.
  (call-next-method generic-function method
                    (cons (maybe-make-tree-specializer-form (car specializer-names))
                          (cdr specializer-names))
                    environment))

;;; `compute-discriminating-function' does this for us now...

(defmethod compute-applicable-methods-using-classes
    ((generic-function abstract-network-tree-generic-function) classes)
  "No cache permitted right now."
  (call-next-method))

;;; `compute-discriminating-function' does this for us now...

(defmethod compute-applicable-methods
    ((generic-function abstract-network-tree-generic-function) args)
  (call-next-method))

;;; `compute-discriminating-function' does this for us now...
#+ ()
(defmethod compute-applicable-methods
    ((generic-function network-tree-generic-function) args)
  (call-next-method generic-function args))


(defmethod compute-effective-method
    ((generic-function network-tree-generic-function)
     method-combination applicable-methods)
  ;; Nothing special yet
  (call-next-method))


(let ((*network-tree-remaining* nil))
  (declare (special *network-tree-remaining*))
  (defmethod compute-discriminating-function
      ((generic-function network-tree-generic-function))
    (let ((it (call-next-method)))
      (declare (type function it))
      (lambda (&rest args)
        (declare (dynamic-extent args))
        (multiple-value-bind (command *network-tree-remaining*)
            (first-command-word (car args))
          (declare (special *network-tree-remaining*))
          (setf (car args) (the network-tree-node
                             (tree-generic-direct-node *network-tree-nodes* command)))
          (apply it args)))))


  (defmethod make-method-lambda
      ((generic-function network-tree-generic-function) method expression environment)
    (let ((result (call-next-method)))
      `(lambda (args next-methods)
         (labels ((remaining-parameters ()
                    (declare (special *network-tree-remaining*))
                    (the string *network-tree-remaining*))
                  (next-node ()
                    (let ((*network-tree-nodes* (car args)))
                      (apply #',(generic-function-name generic-function)
                             (remaining-parameters)
                             (cdr args)))))
           (declare (ignorable  (function next-node)
                                (function remaining-parameters)))
           (,result args next-methods))))))

(let ((*network-tree-remaining* nil))
  (declare (special *network-tree-remaining*))
  (defmethod compute-discriminating-function
      ((generic-function slow-network-tree-generic-function))
    (let ((it (call-next-method)))
      (declare (type function it))
      (lambda (&rest args)
        (declare (dynamic-extent args))
        (multiple-value-bind (command *network-tree-remaining*)
            (first-command-word (car args))
          (declare (special *network-tree-remaining*))
          (setf (car args) (the network-tree-node
                             (tree-generic-direct-node *network-tree-nodes* command)))
          (apply it args)))))


  (defmethod make-method-lambda
      ((generic-function slow-network-tree-generic-function) method expression environment)
    (let ((result (call-next-method)))
      `(lambda (args next-methods &rest more)
         (labels ((remaining-parameters ()
                    (declare (special *network-tree-remaining*))
                    (the string *network-tree-remaining*))
                  (next-node ()
                    (let ((*network-tree-nodes* (car args)))
                      (apply #',(generic-function-name generic-function)
                             (remaining-parameters)
                             (cdr args)))))
           (declare (ignorable  (function next-node)
                                (function remaining-parameters)))
           (apply ,result args next-methods more))))))

#+ ()
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