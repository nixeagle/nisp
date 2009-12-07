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