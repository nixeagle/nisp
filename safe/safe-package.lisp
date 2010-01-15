(in-package :nisp-safe)

;;; utilities first



;;; implentation

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
  "If you hand a safe package in, you get that package back."
  (declare (ignore owner))
  ;; Not 100% positive this is the best behavior, but better then the
  ;; previous behavior that tried to make a new safe package.
  package)

(defvar *populate-functions* '(populate-safe-package-closures)
  "List of functions to populate packages with")
(defun populate-functions (package)
  "Populate package with functions"
  (dolist (it *populate-functions*)
    (funcall (symbol-function it) package)))

(defmethod create-safe-package ((package package) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package package
                             :owner owner)))
    (package-use-from safe (safe-package-use safe))
    (populate-functions safe)
    safe))
(defmethod create-safe-package ((package string) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package (make-empty-package package)
                             :owner owner)))
    (package-use-from safe (safe-package-use safe))
    (populate-functions safe)
    safe))
(defmethod create-safe-package ((package symbol) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package (make-empty-package package)
                             :owner owner)))
    (package-use-from safe (safe-package-use safe))
    (populate-functions safe)
    safe))

(defgeneric get-package (package-designator)
  (:documentation
   "Find the package given a safe-package or a string and return that package."))
(defmethod get-package ((package safe-package))
  (get-package (safe-package package)))

(defmethod get-package (package-designator)
  (find-package package-designator))


(defgeneric package-use-from (package-designator import-from)
  (:documentation
   "Import into PACKAGE the external symbols of IMPORT-FROM."))

(defmethod package-use-from (package-designator
                             import-from)
  ;; Do not specialize on import-from here.
  (use-package import-from (get-package package-designator)))



(defgeneric remove-package (safe-package package-name)
  (:documentation "remove symbols from another package"))


(defgeneric clear-safe-package (safe-package))

(defgeneric delete-safe-package (safe-package)
  ;; This is pretty ineffeciant if given a safe-package, however its not
  ;; that big of a deal to do 3 or 4 calls here as deleting a package is
  ;; something done fairly rarely.
  (:documentation "Given a package related object, delete the package."))
(defmethod delete-safe-package ((package safe-package))
  (delete-safe-package (safe-package package)))
(defmethod delete-safe-package ((package package))
  (delete-safe-package (package-name package)))
(defmethod delete-safe-package (name)
  (delete-package name))

(defmethod (setf reset) ((package safe-package))
  "Reset SAFE-PACKAGE by reloading all symbols."
  (delete-package (safe-package package))
  (setf (safe-package package) 
        (make-empty-package (concatenate 'string "SAFE-" (safe-package-owner package))))
  (package-use-from package (safe-package-use package))
  (populate-functions package))

(defgeneric shadowing-intern (package object)
  (:documentation "intern a new copy of object and setting that copy to the value of the other package's object."))

(defmethod shadowing-intern ((safe-package safe-package) (symbol symbol))
  ;; It is critical that the old symbol be shadowed.
  (with-package (safe-package safe-package)
    (shadowing-import symbol (safe-package safe-package))
    (find-symbol (symbol-name symbol))))  
