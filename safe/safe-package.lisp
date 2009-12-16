(in-package :nisp-safe)

(defparameter *prepared-safe-packages*
  '(:safe-arithmetic
    :safe-arithmetic-trig
    :safe-arithmetic-comparision
    :safe-arithmetic-type-manipulation
    :safe-arithmetic-boole
    :safe-arithmetic-implentation-constants
    :safe-arithmetic-random
    :nisp-safe-introspect
    :safe-external
    :safe-closure
    :safe-testing!)
  "Listing of packages that have been prepared or deemed to be safe.
This is the default list, the idea is depending on the situation mix and
match what capacities you want to allow untrusted code to do.
On the todo list is to create a class that allows multiple instances and
tracks currently interned packages and whatnot.")

(defclass safe-package ()
  ((package :accessor safe-package
            :initarg :package)
   (creation-time :initform (get-universal-time)
                  :accessor package-creation-time)
   (readtable :accessor safe-package-readtable
              :initform (make-readtable))
   (owner :accessor safe-package-owner
          :initarg :owner)
   (use :accessor safe-package-use
        :initform *prepared-safe-packages*))
  (:documentation "Container class for handling safe packages and doing various operations with them."))

(defun make-safe-package (name)
  "Should make a totally independent package. All normal safe stuff
 should be in this by default.
"
  (with-package (make-empty-package name)
    (cl::use-package *prepared-safe-packages*)
    *package* ) ) 

(defmacro with-safe-package (name &body body)
  "Use the given package name to read some code."
  `(with-package ,name
     (with-safe-readtable
       ,@body)))

(defgeneric create-safe-package (package &optional owner)
  (:documentation "Make a brand new safe package.

The optional OWNER parameter defines who owns the package. There is no restriction on what owner can be, but a string is suggested at this time. In the future this parameter may be removed! Its ugly and a kludge."))

(defmethod create-safe-package ((package safe-package) &optional owner)
  (declare (ignore owner))
  ;; If we have a package, why do we need to create a new one?
  (setf (safe-package package)
        (make-empty-package (concatenate 'string "SAFE-"
                                    (safe-package-owner package))))
   (package-use-from package (safe-package-use package)))
(defmethod create-safe-package ((package package) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package package
                             :owner owner)))
    (package-use-from safe (safe-package-use safe))
    (populate-safe-package-closures safe)
    safe))
(defmethod create-safe-package ((package string) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package (make-empty-package package)
                             :owner owner)))
    (package-use-from safe (safe-package-use safe))
    (populate-safe-package-closures safe)
    safe))

(defgeneric get-package (package-designator)
  (:documentation
   "Find the package given a safe-package or a string and return that package."))
(defmethod get-package ((package safe-package))
  (get-package (safe-package package)))

(defmethod get-package (package-designator)
  (find-package package-designator))


(defgeneric package-use-from (package import-from)
  (:documentation
   "Import into PACKAGE the external symbols of IMPORT-FROM."))

(defmethod package-use-from ((package safe-package)
                                     import-from)
  ;; Do not specialize on import-from here.
  (package-use-from (safe-package package)
                            import-from))

(defmethod package-use-from ((package package)
                                import-from)
  (use-package import-from package))

(defmethod package-use-from ((package string)
                             import-from)
  (package-use-from (find-package package)
                       import-from))

(defmethod package-use-from ((package symbol)
                             import-from)
    (package-use-from (find-package package)
                       import-from))



(defgeneric remove-package (safe-package package-name)
  (:documentation "remove symbols from another package"))


(defgeneric clear-safe-package (safe-package))

(defgeneric delete-safe-package (safe-package))
(defmethod delete-safe-package ((package safe-package))
  (delete-package (package-name (safe-package package))))
(defmethod delete-safe-package ((name string))
  (delete-package name))


(defgeneric safe-package-intern (package object)
  (:documentation "intern a new copy of object and setting that copy to the value of the other package's object."))

(defmethod safe-package-intern ((safe-package safe-package) (symbol symbol))
  ;; It is critical that the old symbol be shadowed. Please note that
  ;; we do not set a value to the new symbol in this function.
  (shadow symbol (safe-package safe-package))
  (let ((new-symbol
         (intern (concatenate 'string (symbol-name symbol))
                 (safe-package safe-package))))
    new-symbol))