(defpackage #:nisp
  (:use :cl
        #+5am :5am))

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



(in-package :nisp)

(5am:def-suite all-tests
    :description "Top level test suite")

(in-package :nispbot-config)

(5am:def-suite config-suite
    :in nisp::all-tests)

(in-package :nisp-safe)
(5am:def-suite safe-suite
    :in nisp::all-tests)

(in-package :nisp-asdf)
(5am:def-suite asdf-suite
    :in nisp::all-tests)

(in-suite asdf-suite)

(defun list-system-components (system-name)
  "Given an asdf system, list all components"
  (asdf:module-components (asdf:find-system system-name)))

(test list-system-components
  (is (listp (list-system-components :nisp))
      "Expect a list"))
