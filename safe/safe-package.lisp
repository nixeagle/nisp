(in-package :nisp-safe)



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


(defgeneric create-safe-package (package &optional owner)
  (:documentation "Make a brand new safe package.

The optional OWNER parameter defines who owns the package. There is no restriction on what owner can be, but a string is suggested at this time. In the future this parameter may be removed! Its ugly and a kludge."))

(defmethod create-safe-package ((package safe-package) &optional owner)
  (declare (ignore owner))
  ;; If we have a package, why do we need to create a new one?
  (setf (safe-package package)
        (make-empty-package (concatenate 'string "SAFE-"
                                    (safe-package-owner package))))
   (add-package package (safe-package-use package)))
(defmethod create-safe-package ((package package) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package package
                             :owner owner)))
    (add-package safe (safe-package-use safe))
    (populate-safe-package-closures safe)
    safe))
(defmethod create-safe-package ((package string) &optional owner)
  (let ((safe (make-instance 'safe-package
                             :package (make-empty-package package)
                             :owner owner)))
    (add-package safe (safe-package-use safe))
    (populate-safe-package-closures safe)
    safe))


(defgeneric add-package (safe-package package-name)
  (:documentation "add symbols from another package"))
(defmethod add-package ((package safe-package) package-name)
  (with-safe-package (safe-package package)
    (use-package package-name (safe-package package))))


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