(defpackage #:loki-util
  (:use :cl))

(in-package :loki-util)

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