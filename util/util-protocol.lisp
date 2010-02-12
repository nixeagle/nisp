(in-package :common-lisp-user)
(defpackage #:nisp.util-protocol
  (:use :cl :iterate :nisp.util-types)
  (:export :read-sequence-until))
(in-package :nisp.util-protocol)
(defgeneric read-sequence-until (object target &key limit non-blocking)
  (:documentation "Read OBJECT up to TARGET.

Stop reading OBJECT if LIMIT is reached."))

;;; Guidance taken from cl-irc on this implentation. However they use
;;; loop and treat this as a function not a generic method/methods.
(defmethod read-sequence-until ((char-stream stream) (target character)
                                &key limit non-blocking)
  "Read CHAR-STREAM upto TARGET."
  ;; Need to actually handle LIMIT and NON-BLOCKING or handle
  ;; NON-BLOCKING as part of the stream type.
  (declare (type (or positive-fixnum nil) limit)
           (type boolean non-blocking)
           (ignore limit non-blocking))
  (iter (for char :in-stream char-stream :using #'read-char-no-hang)
        (until (or (null char) (char= target char)))
        (collect char)))