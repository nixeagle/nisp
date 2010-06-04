(in-package :nisp.network-tree)

(defun parse-method-lambda-expression-body (lambda-form)
  "Try to find the body of LAMBDA-FORM.

Returns the body as the first value and the stuff before the body as the
second value."
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




(defun pretend-splits (string)
  (iter (repeat 1)
    (for (values res next-start)
         initially (test-first-command-word string)
         then (test-first-command-word string next-start))
        (collect res)
        (while next-start)
        ))
(defun pretend-splits/2 (string)
  (declare (optimize (speed 3) (debug 1) (safety 0)))
  (iter (for (values new rest)
             initially (first-command-word string)
             then (first-command-word rest))
        (repeat 3)
        (until (string= rest ""))))
(declaim (inline test-first-command-word))
(defun test-first-command-word (command-string &optional (start-index 0) (seperater #\Space))
  "Extract the first segment in COMMAND-STRING before SEPERATER.

Second return value is what is left after removing the segment."
  (declare (simple-string command-string)
           (character seperater)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((space-index (position seperater command-string :start start-index)))
    (declare (type (or null non-negative-fixnum) space-index))
    (if space-index
        (values (subseq command-string start-index space-index)
                (1+ space-index))
        (values (if (zerop start-index)
                    command-string
                    (subseq command-string start-index))
                nil))))
(defun first-command-word (command-string &optional (seperater #\Space)
                           (start-index 0))
  "Extract the first segment in COMMAND-STRING before SEPERATER.

Second return value is what is left after removing the segment."
  (declare (type simple-string command-string)
           (type character seperater)
           (optimize (speed 3) (safety 0) (debug 1)))
  (let ((space-index (position seperater command-string :start start-index)))
    (declare (type (or null non-negative-fixnum) space-index))
    (if space-index
        (values
         (subseq command-string start-index space-index)
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

#+ ()
(defgeneric ensure-network-node-symbol (symbol)
  (:documentation "Make sure SYMBOL exists in `+network-tree-symbols-package+'."))

(defun ensure-network-node-symbol (arg)
  (if (stringp arg)
      arg
      (princ-to-string arg)))

#+ ()
(defmethod ensure-network-node-symbol (arg)
  "Make a symbol out of ARG by `format-symbol'."
  (string-upcase (format "~A" arg)))
#+ ()
(defmethod ensure-network-node-symbol ((symbol symbol))
  "Make SYMBOL a list and recall."
  (call-next-method))
#+ ()
(defmethod ensure-network-node-symbol ((symbol-as-string string))
  "Split by spaces, then intern SYMBOLS as normal."
  (declare (type network-node-string symbol-as-string))
  (string-upcase symbol-as-string))


(defgeneric ensure-network-node-symbols (symbols)
  (:documentation "Return a list of symbols instead of just one."))

(defmethod ensure-network-node-symbols ((args list))
  (mapcar #'ensure-network-node-symbol args))
(defmethod ensure-network-node-symbols ((args symbol))
  (ensure-network-node-symbols (ensure-list args)))
(defmethod ensure-network-node-symbols ((symbols-as-string string))
  (split-sequence #\Space
                  (string-upcase symbols-as-string)
                  :remove-empty-subseqs t))

;;;}}}

;;;{{{ Tree classes
(defclass network-tree-parent ()
  (#+ () (parent :initarg :parent
           :reader network-tree-parent))
 #+ () (:default-initargs :parent nil))

(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
  (defstruct (tree-generic-direct-nodes
               (:conc-name))
    (tree-generic-direct-nodes (make-hash-table :test 'equalp
                                                #+sbcl :weakness
                                                #+ccl :weak
                                                #+ (or sbcl ccl) :value)
                               )))

(declaim (inline tree-generic-direct-node))
(defun tree-generic-direct-node (tree arg)
  (declare (type simple-string arg)
           (type tree-generic-direct-nodes tree)
           (optimize (speed 3) (safety 0) (debug 0)))
  (gethash arg (tree-generic-direct-nodes tree)))

(defclass network-tree-generic-function
    (#+sbcl
     cl:standard-generic-function
     #-sbcl standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'tree-method)))

(defstruct (network-tree-node (:include tree-generic-direct-nodes)
                              (:conc-name))
  )


(defclass network-tree-method (standard-method network-tree-parent)
  ((methods :type list :reader network-tree-method-methods
            :initform ()))
  #+ccl (:default-initargs :closer-patch t))
(defclass tree-method (network-tree-method)
  ())

#+ () (defmethod print-object ((obj network-tree-node) stream)
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
                   (make-network-tree-node)
                   ))
         (cdr symbols))
        node)))

(defun intern-network-tree-node (symbols)
  "Intern a unique specializer for TREE for SYMBOLS.

SYMBOLS represents a path starting at the root of TREE and going down one
symbol at a time through repeated hash-tables until the last of SYMBOLS is
reached.

SYMBOLS may be a list of symbols, a string of space seperated words that
is translated into a list of symbols."
  (declare (type string symbols))
  (%intern-network-tree-node *network-tree-nodes* (ensure-network-node-symbols symbols)))

(defun find-network-tree-node (name &key (nodes *network-tree-nodes*))
  "Find a node called NAME."
  (loop for node-name in (ensure-network-node-symbols name)
     for current-node = nodes then next-node
     for next-node = (gethash node-name (tree-generic-direct-nodes current-node))
     finally (return next-node)))

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
  ;; If it is a list, we assume it is a tree specializer.
  (if (listp (car specializer-names))
      (call-next-method generic-function method
                        (cons (maybe-make-tree-specializer-form (car specializer-names))
                              (cdr specializer-names))
                        environment)
      (call-next-method)))
(defparameter *args* nil)
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
          (apply it (the network-tree-node
                             (tree-generic-direct-node *network-tree-nodes* command))
                (cdr args))))))


  (defmethod make-method-lambda
      ((generic-function network-tree-generic-function) method expression environment)
    (multiple-value-bind (body lambda-args declarations)
        (parse-method-lambda-expression-body expression)
      (let ((expression
             `(lambda ,lambda-args ,@declarations
                      (labels ((test () 1)
                             (remaining-parameters ()
                               (declare (special *network-tree-remaining*))
                               (the string *network-tree-remaining*))
                             (next-node (&optional (remaining-parameters (remaining-parameters)))
                               (let ((*network-tree-nodes* ,(car lambda-args)))
                                 (funcall #',(generic-function-name generic-function)
                                          remaining-parameters
                                        ,@(cdr lambda-args)))))
                        (declare (ignorable (function test)
                                            (function next-node)
                                            (function remaining-parameters)))
                        ,@body))))
        (let  ((lamb (call-next-method generic-function method expression environment)))
          lamb)))))

(defmethod make-load-form ((self network-tree-node) &optional env)
  (declare (ignore env))
  (values `(intern-network-tree-node ,(network-tree-node-object self))
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
