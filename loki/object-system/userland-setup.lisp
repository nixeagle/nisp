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
        (eq |self| other)))

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