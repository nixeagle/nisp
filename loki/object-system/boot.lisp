(defpackage #:loki-object-system
  (:use :cl :alexandria :bt :anaphora)
  (:shadow :method :call-method :make-method)
  (:export #:loki-object))

(in-package :loki-object-system)

(defvar *world-lock* (make-lock "loki-world-lock")
  "Master lock for the loki system.

If thread protection is required for some operation and that operation is
not easily localizable or the relevant protocols are still too raw, this
lock should be held.")

(defstruct (object (:conc-name nil))
  "All programmer level objects in loki are instances of this object."
  (object-lock (make-lock "instance lock")
               #+sbcl :type #+sbcl sb-thread:mutex
               #+ccl :type #+ccl ccl:lock)
  direct-mimics
  direct-imitators
  (direct-cells (make-hash-table :test 'equalp)
                :type hash-table))

(defstruct (data-mixin (:conc-name nil)
                       (:include object)))

(defstruct (number-mixin (:conc-name nil)
                         (:include data-mixin))
  "The top of the numberical tower.")

(defstruct (real-mixin (:conc-name nil)
                       (:include number-mixin)))

(defstruct (rational-mixin (:conc-name nil)
                           (:include real-mixin)))

(defstruct (ratio-mixin (:conc-name nil)
                        (:include rational-mixin)))

(defstruct (string-object (:conc-name string-)
                          (:include data-mixin))
  (data "" :type string))

(defstruct (number-object (:conc-name number-)
                          (:include number-mixin))
  (data 0 :type number))

(defstruct (decimal-object (:conc-name decimal-)
                           (:include real-mixin))
  (data 0 :type real))

(defstruct (complex-object (:conc-name complex-)
                           (:include number-mixin))
  (data 0 :type (or rational real integer complex)))

(deftype infinity ()
  '(member :loki-infinity :loki-negative-infinity))

(defstruct (infinity-object (:conc-name infinity-)
                            (:include ratio-mixin))
  (data :loki-infinity :type infinity))

(defstruct (integer-object (:conc-name integer-)
                          (:include rational-mixin))
  (data 0 :type integer))
(defstruct (symbol-object (:conc-name symbol-)
                          (:include data-mixin))
  (data :undefined-symbol :type symbol))

(defstruct (method-object
             (:conc-name method-)
             (:include data-mixin)
             (:constructor)
             (:constructor create-method-object
                           (&key direct-mimics direct-imitators
                                 direct-cells lambda-list forms
                                 &aux (function (compile nil
                                                         `(lambda ,lambda-list
                                                            ,@forms)))))
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


(defmethod make-load-form ((self method-object) &optional env)
  (declare (ignore env))
  `(make-method-object :direct-mimics ',(direct-mimics self)
                       :direct-cells ',(direct-cells self)
                       :argument-list ',(method-lambda-list self)
                       :forms ',(method-forms self)
                       :function (lambda ,(method-lambda-list self)
                                   ,@(method-forms self))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun object-lock-forms (&rest loki-objects)
    "Returns list of ((loki-object-lock LOKI-OBJECT)...)."
    (mapcar (lambda (obj) `(object-lock ,obj))
            loki-objects))

  (defun loki-nested-lock-forms (lock-function-symbol places forms)
    (let ((forms-done-p nil))
      (reduce (lambda (old new)
                `(,lock-function-symbol (,new)
                                        ,@(if forms-done-p
                                              (list old)
                                              (progn (setq forms-done-p t)
                                                     old))))
              (reverse places) :initial-value forms))))

(defmacro with-loki-object-locks-held ((&rest loki-objects) &body body)
  (loki-nested-lock-forms 'with-lock-held
                          (apply #'object-lock-forms loki-objects)
                          body))

(defun add-direct-mimic (object mimic)
  "Make OBJECT directly superclass MIMIC.

Metaprotocol notes:

  It is an error to add a MIMIC that is not derived from `loki-object'.

  It is an error for an OBJECT to superclass itself. In otherwords OBJECT
  and MIMIC may not be the same instance.

  The return value of this function is unspecified.

Thoughts:

  Do we need to lock OBJECT and MIMIC for this operation? If so why? For
  now we lock as a precaution."
  (declare (type object object mimic))
  (with-loki-object-locks-held (object mimic)
    (assert (not (eq object mimic)))
    (pushnew mimic (direct-mimics object) :test 'eq)))

(defun remove-direct-mimic (object mimic)
  (declare (type object object mimic))
  (with-loki-object-locks-held (object mimic)
    (assert (not (eq object mimic)))
    (setf (direct-mimics object)
          (delete mimic (direct-mimics object) :test 'eq))))

(defun add-direct-imitator (object imitator)
  "Mark OBJECT as being imitated by IMITATOR.

Metaprotocol notes:

  Should be same rules as `add-direct-mimic'."
  (declare (type object object imitator))
  (with-loki-object-locks-held (object imitator)
    (assert (not (eq object imitator)))
    (pushnew imitator (direct-imitators object) :test 'eq)))

(defun remove-direct-imitator (object imitator)
  (declare (type object object imitator))
  (with-loki-object-locks-held (object imitator)
    (assert (not (eq object imitator)))
    (setf (direct-imitators object)
          (delete imitator (direct-imitators object) :test 'eq))))

(defun add-direct-cell (object name value)
  (declare (type object object value)
           (type string name))
  (with-loki-object-locks-held (object)
    (setf (gethash name (direct-cells object)) value)))

(defun remove-direct-cell (object name)
  (declare (type object object)
           (type string name))
  (with-loki-object-locks-held (object)
    (remhash name (direct-cells object))))

(defun direct-cell (object name)
  "Get the cell value in OBJECT identified by NAME."
  (declare (type object object)
           (type string name))
  (the (or object null) (gethash name (direct-cells object))))

(defsetf direct-cell add-direct-cell)

(defun call-method (object &rest args)
  "Apply ARGS to OBJECT's `method-function'."
  (declare (type method-object object))
  (apply (method-function object) args))

(defmacro make-method (lambda-list &body body)
  `(create-method-object :lambda-list ',lambda-list
     :forms ',body))

(defvar *base* (make-object)
  "Root of a loki Package.

Scott calls packages a RunTime, and of course packages are not
implemented at this time. [2010-03-17 Wed 07:41]")

(add-direct-cell *base* "hash" (make-method (self)
                                 "Hash on Base is as if `eq' is used."
                                 (declare (type object self))
                                 (sxhash self)))

(add-direct-cell *base* "cell"
                 (make-method (self cell-name)
                   "Return a cell as if by `cell'."
                   (declare (type object self)
                            (type (or string-object symbol-object) cell-name))
                   (etypecase cell-name
                     (string-object (cell self (string-data cell-name)))
                     (symbol-object (cell self (symbol-name (symbol-data cell-name)))))))

(add-direct-cell
 *base* "=="
 (make-method (self other)
   "True iff SELF is `eq' to OTHER."
   (declare (type object self other))
   (eq self other)))


(defun write-readably (object &optional (stream *standard-output*))
  (let ((*print-readably* t)
        (*print-pretty* nil)
        (*print-circle* t))
    (prin1 object stream)))

(defun write-readably-to-string (object)
  (let ((*print-readably* t)
        (*print-pretty* nil)
        (*print-circle* t))
    (prin1-to-string object)))

(defvar *Ground* (make-object)
  "Usually between `Base' and most anything else.")

(add-direct-mimic *Ground* *Base*)
(add-direct-imitator *Base* *Ground*)

(defvar *origin* (make-object)
  "Mimics `Ground' and is where new stuff in loki comes from.")

(add-direct-mimic *origin* *ground*)
(add-direct-imitator *origin* *ground*)

(defun cell (object name)
  "Recursively find cell on OBJECT by NAME."
  (or (direct-cell object name)
      (loop for mimic in (direct-mimics object)
         for result = (cell mimic name)
           when result return result)))