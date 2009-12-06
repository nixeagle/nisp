

(in-package :nispbot-tests)



(defpackage #:introspection-tests
  (:use ))

(def-suite introspection-tests
    :in all-tests
    :description "Test that introspection functions behave as they should")

(def-suite function-lambda-list/suite
    :in introspection-tests
    :description "Verify that lambda list generation is correct.")

(in-suite function-lambda-list/suite)

(test invalid-string
  (is (stringp ())))

(def-suite error-handling-tests :in nisp::all-tests)
(in-suite error-handling-tests)