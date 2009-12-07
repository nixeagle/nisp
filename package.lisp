(defpackage #:nisp-asdf
  (:use :cl #+5am :5am))

(defpackage #:nisp-introspect
  (:use :cl #+5am :5am)
  (:export #:function-lambda-list))


(defpackage #:nispbot-config
  (:use :common-lisp #+5am :5am
        )
  (:export *channel* *nickname* *eighthbit*))

(defpackage #:nisp-safe
  (:use :common-lisp #+5am :5am))

(defpackage #:nisp-random
  (:use :cl :5am))
(in-package :nisp-random)

;;; source taken from swank and modified to do what I wanted to do.

(defun nisp-pprint-eval (string)
  (swank::with-buffer-syntax ()
    (let* ((stand (make-string-output-stream))
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