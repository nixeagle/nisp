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
  (signals type-error (list-system-components 1))
)

