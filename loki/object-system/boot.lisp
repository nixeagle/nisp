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
                :type hash-table)
  (docstring "" :type string))

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

(defstruct (rational-object (:conc-name rational-)
                            (:include real-mixin))
  (data 0 :type rational))
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

(defparameter *base* (make-object)
  "Root of a loki Package.

Scott calls packages a RunTime, and of course packages are not
implemented at this time. [2010-03-17 Wed 07:41]")
(defparameter *Ground* (make-object)
  "Usually between `Base' and most anything else.")
(defparameter *origin* (make-object)
  "Mimics `Ground' and is where new stuff in loki comes from.")
(defvar *context* *Ground*
  "Defaults to `*Ground*', but this can change.")
(defvar *surrounding-context* nil
  "Supposed to be the context at the call site.")
(defvar *receiver* *Ground*
  "Known as self or @ in loki.")
(defvar *current-message* nil)
(defvar *this-method* nil
  "The current method being invoked.")


(defun cell (object name)
  "Recursively find cell on OBJECT by NAME."
  (or (direct-cell object name)
      (loop for mimic in (direct-mimics object)
         for result = (cell mimic name)
           when result return result)))