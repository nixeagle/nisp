(in-package :nisp-asdf)
(5am:def-suite asdf-suite
    :in nisp::all-tests)

(in-suite asdf-suite)

(deftestsuite nisp-asdf-suite () ())

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

(deftestsuite list-system-components (nisp-asdf-suite)
  ()
  :test (pass-keyword-with-valid-package-name
         (ensure-same (integerp (list-system-components :nisp)) t))
  :test (pass-string-with-valid-package-name
         (ensure (listp (list-system-components "nisp"))))
  :test (pass-multiple-args-in-a-list
         (ensure (listp (list-system-components '(:nisp :nispbot)))))
  :test (pass-integer
         (ensure-condition 'simple-type-error
           (list-system-components 1)))
  :test (pass-nil
         (ensure-condition 'asdf::missing-component
           (list-system-components nil)))
  (:test 
    (pass-void-package-keyword
     (:documentation "testing")
     (ensure-condition 'asdf::missing-component
       (list-system-components :this-does-not-exist-as-a-package)
       ) )
    )
  :documentation "list-system-components is a generic function.

The point of all the tests in this suite is to verify that the function
handles message passing as expected. We verify that strings, lists and
keywords all return valid results.")

#+ (or)
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