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


(in-package :nisp-asdf)
(5am:def-suite asdf-suite
    :in nisp::all-tests)

(in-suite asdf-suite)


(defgeneric list-system-components (systems)
  (:method (systems)
    (check-type systems (or string keyword cons)))
  (:documentation "Return components of SYSTEM-NAME"))

(defmethod list-system-components ((system-names cons))
  (mapcar #'list-system-components system-names))

(defmethod list-system-components ((system-name symbol))
  (asdf:module-components
   (asdf:find-system system-name)))

(defmethod list-system-components ((system-name string))
  (asdf:module-components (asdf:find-system system-name)))


(test list-system-components
  (is (listp (list-system-components :nisp))
      "Expect a list")
  (is (listp (list-system-components '(:nisp :nispbot)))
      "Should be able to take more then one arg.")
  (signals (simple-type-error) (list-system-components 1))
  (signals (asdf::missing-component) (list-system-components nil)
           "Nil has nothing we are interested in.")
  (signals
      (asdf::missing-component)
    (list-system-components :this-does-not-exist-as-a-package))
  (is (listp (list-system-components "nisp"))))
 

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