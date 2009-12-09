(defpackage #:nisp-random
  (:use :cl :5am))
(in-package :nisp-random)

;;; source taken from swank and modified to do what I wanted to do.

(defun nisp-pprint-eval (string)
  (swank::with-buffer-syntax ()
    (let* ((5am:*run-test-when-defined* t)
           (stand (make-string-output-stream))
           (trace (make-string-output-stream))
           (*standard-output* stand)
           (*trace-output* trace)
           (form (read-from-string string))
           (values (multiple-value-list
                    (eval form))))
      (list string
            (get-output-stream-string stand)
            (get-output-stream-string trace)
            (nisp-pprint values)))))

(defun nisp-pprint (values)
  (swank::with-buffer-syntax ()
    (swank::with-bindings swank::*swank-pprint-bindings*
      (with-output-to-string (*standard-output*)
        (dolist (o values)
          (pprint o))))))

(defun matches-list (reg &rest strings)
  (let ((ret ()))
    (dolist (str strings ret)
      (push 
       (second (multiple-value-list 
                (cl-ppcre:scan-to-strings reg str))) ret))))