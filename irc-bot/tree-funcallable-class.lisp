(in-package :nisp.i)

;;;{{{ Tree classes
(defclass tree-generic-direct-nodes ()
  ((direct-nodes :initform (make-hash-table :test 'eq :weakness :value)
                 :reader tree-generic-direct-nodes)))

(defclass tree-generic-function (standard-generic-function
                                 tree-generic-direct-nodes)
  ()
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class (find-class 'tree-method)))


(defclass tree-method (standard-method)
  ())

(defclass tree-specializer (eql-specializer
                            tree-generic-direct-nodes)
  ((parent :reader tree-specializer-parent
           :documentation "Pointer to the parent specializer of this one.")))


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

(defgeneric preprocess-arglist (generic-function args)
  (:documentation
   "Modify ARGS to make them suiable for computing applicable methods."))
(defmethod preprocess-arglist ((generic-function tree-generic-function) args)
  (cons (car (ensure-tree-symbols (car args))) (cdr args)))

(defun %intern-tree-specializer (tree symbols)
  (declare (type tree-generic-direct-nodes tree)
           (type list symbols)
           (optimize (speed 3) (safety 0) (debug 1)))
  (the tree-specializer
    (if symbols
        (%intern-tree-specializer
         (or (gethash (car symbols) (tree-generic-direct-nodes tree))
             (setf (gethash (car symbols) (tree-generic-direct-nodes tree))
                   (make-instance 'tree-specializer :object (car symbols))))
         (cdr symbols))
        tree)))

(defun intern-tree-specializer (tree symbols)
  "Intern a unique specializer for TREE for SYMBOLS.

SYMBOLS represents a path starting at the root of TREE and going down one
symbol at a time through repeated hash-tables until the last of SYMBOLS is
reached.

SYMBOLS may be a list of symbols, a string of space seperated words that
is translated into a list of symbols."
  (declare (type tree-generic-direct-nodes tree)
           (type (or string list keyword) symbols))
  (%intern-tree-specializer tree (ensure-tree-symbols symbols)))

(defun maybe-make-tree-specializer-form (generic-function specializer-name)
  ;; We don't actually check right now, instead just making the correct
  ;; form.
  ;;
  ;; This _must_ return a form that creates the specializer, never the
  ;; specializer itself. This solves a bug related to SB-PCL::LOAD-DEFMETHOD.
  ;;
  ;; We may need to consider not including anything with sharpsign < in
  ;; the printed form, so the generic function for our case might need to
  ;; be #',(generic-function-name it)
  `(intern-tree-specializer ,generic-function ,@(cdr specializer-name)))

;;; Now we need to make a specializer class. Most of this is from sbcl's
;;; boot.lisp `real-make-method-specializers-form'.
(defmethod sb-pcl:make-method-specializers-form
    ((generic-function tree-generic-function)
     method specializer-names environment)
  ;; We always assume a specializer exists in the first element of
  ;; SPECIALIZER-NAMES. We certainly can do better, but this works.
  (cons 'list
        (cons (maybe-make-tree-specializer-form generic-function (car specializer-names))
              (cdr (call-next-method generic-function method
                                     (cdr specializer-names)
                                     environment)))))

(defmethod compute-applicable-methods-using-classes
    ((generic-function tree-generic-function) classes)
  "No cache permitted right now."
  (values classes nil))

;;; We don't need this around method right now, we don't even do anything
;;; with the primary!
#+ ()
(defmethod compute-applicable-methods :around
    ((generic-function tree-generic-function) args)
  (call-next-method generic-function args))

(defmethod compute-applicable-methods
    ((generic-function tree-generic-function) args)
  ;; Nothing special yet
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
    (describe it)
    it))


(defmethod make-method-lambda
    ((generic-function tree-generic-function) method expression environment)
  (declare (ignore environment))
  (call-next-method))

(defmethod make-load-form ((self tree-specializer) &optional env)
  (declare (ignore env))
  (values (intern-tree-specializer #'test-tree-generic-function
                                   (eql-specializer-object self))
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