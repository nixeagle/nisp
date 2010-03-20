(in-package :loki-object-system)



(setf (direct-cell *base* "kind") (make-string-object :data "Base"))
(setf (direct-cell *base* "inspect") (make-string-object :data "Base"))
(setf (direct-cell *base* "notice") (make-string-object :data "Base"))

(setf (direct-cell *base* "hash")
      (make-method ()
        "Hash on Base is as if `eq' is used.

This does not seem to be returning the right hash for two otherwise eq
loki objects."
        (sxhash |self|)))

(setf (direct-cell *base* "cell")
      (make-method (cell-name)
        "Return a cell as if by `cell'."
        (etypecase cell-name
          (string-object (cell |self| (string-data cell-name)))
          (symbol-object (cell |self| (symbol-name (symbol-data cell-name)))))))

(setf (direct-cell *base* "==")
      (make-method (other)
        "True iff SELF is `eq' to OTHER."
        (declare (type object other))
        (eq |self| other)))

(setf (direct-cell *base* "=")
      (make-method (place value)
        "Set PLACE on @ to VALUE."
        (setf (direct-cell @ place) value)))

;;;object context message reciever
;;; reciever is @ or self
;;;
;;; currentMessage is message that initiated activation of method
;;;
;;; surroundingContext object that represents the context where this
;;; method was called from

(add-direct-mimic *Ground* *Base*)
(add-direct-imitator *Ground* *Base*)

(setf (direct-cell *Ground* "Ground") *Ground*)



(add-direct-mimic *origin* *ground*)
(add-direct-imitator *origin* *ground*)

(defpackage #:loki-user
  (:use))


(define-constant +standard-read-string+ (get-macro-character #\" (copy-readtable nil)))

(defun call-with-loki-readtable (thunk)
  (declare (type function thunk))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (set-macro-character #\" (lambda (stream char)
                               (make-string-object
                                :data
                                (funcall +standard-read-string+ stream char))))
    (let ((*package* (find-package :loki-user)))
      (walk-loki-list (cl:funcall thunk)))))

(defun walk-loki-list (list)
  (declare (type list list))
  (mapcar #'make-simple-data-object
          list))

(defun make-simple-data-object (input)
  (typecase input
    (complex (make-complex-object :data input))
    (integer (make-integer-object :data input))
    (rational (make-rational-object :data input))
    (string (make-string-object :data input))
    (otherwise input)))

(defun integer->ioke (integer)
  (declare (type integer integer))
  )

(defmacro with-loki-syntax (&body body)
  `(call-with-loki-readtable (lambda () ,@body)))