(in-package :nisp.network-tree)

(defun ^find-body (lambda-form)
  "Try to find the body of LAMBDA-FORM.

Returns the body as the first value and the stuff before the body as the
second value."
  ;; Warning: this whole function is a hack and a kludge.
  (let ((lambda-args (second lambda-form))
        declare-forms)
    (labels ((parse (body &optional docstringp)
               (if (or (and (consp (car body))
                            (eq 'declare (caar body)))
                       (and (stringp (car body)) (cdr body)))
                 (progn
                   (push (car body) declare-forms)
                   (parse (cdr body) (or docstringp (stringp (car body)))))
                 body)))
      (values (parse (cddr lambda-form)) lambda-args (reverse declare-forms)))))

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
  (when (stringp string)
    (not (find #\Space string))))

(deftype network-node-string ()
  "String with no spaces."
  '(and string (satisfies network-node-string-p)))

;;;{{{ ensure-network-node-symbol(s)
(defparameter +network-tree-symbols-package+ (find-package :keyword)
  "Package that all tree symbols should get interned into.")

(defgeneric ensure-network-node-symbol (symbol)
  (:documentation "Make sure SYMBOL exists in `+network-tree-symbols-package+'."))
(defmethod ensure-network-node-symbol (arg)
  "Make a symbol out of ARG by `format-symbol'."
  (format-symbol +network-tree-symbols-package+ "~A" arg))
(defmethod ensure-network-node-symbol ((symbol symbol))
  "Make SYMBOL a list and recall."
  (call-next-method))

(defmethod ensure-network-node-symbol ((symbol-as-string string))
  "Split by spaces, then intern SYMBOLS as normal."
  (declare (type network-node-string symbol-as-string))
  (intern (string-upcase symbol-as-string) +network-tree-symbols-package+))

(defgeneric ensure-network-node-symbols (symbols)
  (:documentation "Return a list of symbols instead of just one."))

(defmethod ensure-network-node-symbols ((args list))
  (mapcar #'ensure-network-node-symbol args))
(defmethod ensure-network-node-symbols ((args symbol))
  (ensure-network-node-symbols (ensure-list args)))
(defmethod ensure-network-node-symbols ((symbols-as-string string))
  (ensure-network-node-symbols (split-sequence #\Space
                                       symbols-as-string
                                       :remove-empty-subseqs t)))

;;;}}}

;;;{{{ Tree classes
(defclass network-tree-parent ()
  ((parent :initarg :parent
           :reader network-tree-parent))
  (:default-initargs :parent nil))

(defclass tree-generic-direct-nodes (network-tree-parent)
  ((direct-nodes :initform (make-hash-table :test 'equalp :weakness :value)
                 :reader tree-generic-direct-nodes)))

(defun tree-generic-direct-node (tree arg)
  (declare (type string arg)
           (type tree-generic-direct-nodes tree)
           (optimize (speed 3) (safety 0) (debug 1)))
  (gethash arg (tree-generic-direct-nodes tree)))

(defclass network-tree-generic-function
    (cl:standard-generic-function tree-generic-direct-nodes)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'tree-method)))

(defclass network-tree-node (eql-specializer tree-generic-direct-nodes)
  ())
(defmethod  network-tree-node-object ((obj network-tree-node))
  (eql-specializer-object obj))
(defclass network-tree-method (standard-method network-tree-parent)
  ((methods :type list :reader network-tree-method-methods
            :initform ())))
(defclass tree-method (network-tree-method)
  ())

(defmethod print-object ((obj network-tree-node) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (princ (network-tree-node-object obj) stream)))


;;;}}}


;;;{{{ Interning network-tree nodes
(defvar *network-tree-nodes* (make-instance 'tree-generic-direct-nodes))

(defun %intern-network-tree-node (node symbols)
  (declare (type tree-generic-direct-nodes node)
           (type list symbols)
           ;; We need tail recursion
           (optimize (speed 3) (safety 0) (debug 1)))
  (the network-tree-node
    (if symbols
        (%intern-network-tree-node
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
  (%intern-network-tree-node *network-tree-nodes* (ensure-network-node-symbols symbols)))

;;;}}}

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
#+sbcl
(defmethod sb-pcl:make-method-specializers-form
    ((generic-function network-tree-generic-function)
     method specializer-names environment)
  ;; We always assume a specializer exists in the first element of
  ;; SPECIALIZER-NAMES. We certainly can do better, but this works.
  (call-next-method generic-function method
                    (cons (maybe-make-tree-specializer-form (car specializer-names))
                          (cdr specializer-names))
                    environment))

(let ((*network-tree-remaining* nil))
  (declare (special *network-tree-remaining*))
  (defmethod compute-discriminating-function
      ((generic-function network-tree-generic-function))
    (call-next-method)
    #+ () (let ((it (call-next-method)))
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
    (multiple-value-bind (body lambda-args declarations)
        (^find-body expression)
      (let ((expression
             `(lambda ,lambda-args ,@declarations
                      (flet ((test () 1))
                        (declare (ignorable (function test)))
                        ,@body))))
        (let ((lamb (call-next-method generic-function method expression environment)))
          #+ () (setq @lamb@ lamb)
          #+ () lamb
          lamb))
      ) #+ () (call-next-method)

    #+ ()
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


;;; END
