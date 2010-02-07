(in-package :common-lisp-user)

(defpackage #:nisp.util.packages
  (:use :cl)
  (:export :make-empty-package))

(in-package :nisp.util.packages)

(defgeneric make-empty-package (package if-exists)
  (:documentation "Create empty package PACKAGE."))

(defmethod make-empty-package (package (if-exists symbol))
  "If PACKAGE exists error."
  (declare (type keyword if-exists))
  (make-package package :use '()))

(defmethod make-empty-package :around (package (if-exists (eql :ignore)))
  "Call next method if PACKAGE does not exist."
  (or (find-package package) (call-next-method)))

#+ (and sbcl)
(defmethod make-empty-package :around (package (if-exists (eql :error)))
  "Catch and rethrow error when PACKAGE does not exist.

SBCL (as of 1.0.34) does not release the lock during `find-package'
which results in slime which is on another thread deadlocking. By using
`handler-case' we can unwind the stack (in the process releasing the
lock...) and rethrow the error to prevent the deadlock."
  (handler-case (call-next-method) (error (condition) (error condition))))

(defmethod make-empty-package :before (package (if-exists (eql :supersede)))
  "Delete PACKAGE if it already exists.

The primary method will then make a new empty package."
  (and #+sbcl (find-package package)    ;sbcl bug
       (delete-package package)))

;;; Some testing I did:
;;;
;;; The `make-with-find-package' is roughly 4 times faster.
#+ ()
(defun make-with-handler-case (x)
  (handler-case (make-package (format nil "tast-~A" x)) (error (condition) nil)))
;;; (h::clock (make-with-handler-case (random 100)) 200000)
;;=> ((:TIME 526/125 "4.208") (:AVG 263/12500000 "0.00002104") (:RESULT))

#+ ()
(defun make-with-find-package (x)
  (or (find-package (format nil "test-~A" x))
      (make-package (format nil "test-~A" x))))
;;; (h::clock (make-with-find-package (random 100)) 200000)
;;=> ((:TIME 1269/1000 "1.269") (:AVG 1269/200000000 "0.000006345") (:RESULT . #<PACKAGE "test-20">))