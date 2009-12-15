
(in-package #:nisp-safe)

;; Tests
(deftestsuite root-suite (nisp::root-suite) ())
(deftestsuite base-packages (root-suite)
  ()
  (:setup
   (make-empty-package "test1"))
  (:teardown
   (delete-safe-package-old "test1")))

(deftestsuite test-make-readtable (root-suite)
  ()
  (:documentation "Make sure that a new read table also comes with #\\\: function bound.")
  :test (colon-macro-function-bound?
         (ensure (functionp (get-macro-character #\: (make-readtable))))))

(deftestsuite test-with-safe-readtable (base-packages)
  ()
  (:documentation "Test the readtable stuff with a focus towards attempting to break the assumptions made.")
  :test (modify-readtable
         (:documentation
          "Make sure that inside this form *readtable* is set to
*safe-readtable*")
         (with-safe-readtable
           (ensure (eq *readtable* *safe-readtable*))))
  :test (double-colon
         (:documentation
          "IF YOU SEE THIS:: DO NOT EVEN THINK ABOUT IGNORING IT!

This message means that a function outside of a socalled 'safe' package
was accessed. This test does the following:

1) create an empty package, eg one with no internal symbols
2) runs read-from-string using the package as the *PACKAGE* variable.
3) reads the following string \"cl::+\".
4) If cl::+ is an fbound symbol a critical assumption failed
   or a regression has occured.

AGAIN DO NOT EVEN THINK ABOUT USING WHILE THIS TEST FAILS!")
         (ensure
          (not
           (fboundp
            (with-safe-readtable
              (read-using-package "test1" "cl::+")))))))

(deftestsuite test-*safe-readtable* (root-suite)
    ()
    :documentation "Make sure we get a readtable."
    :test (is-readtable
           (ensure (readtablep *safe-readtable*))))

;;; Tests
(deftestsuite make-safe-package (root-suite)
  ((keyword :test-keyword)
   (string "test-string"))
  (:teardown (delete-safe-package-old keyword)
             (delete-safe-package-old string))
  :test (pass-keyword
         (ensure (packagep (make-safe-package keyword))))
  :test (pass-string
         (ensure (packagep (make-safe-package string)))))


(deftestsuite test-colon-reader (base-packages)
  ()
  (:documentation "Verify that the reader function for #\\\: throws an error when it is supposed to and permits what it is not supposed to throw an error on. Currently this latter part is largely forgotten as we prefer to default to sane (overly restrictive) then too loose with regards to package access.

Please note that at this time we know the reader macro prevents things that it should not prevent. Example(s) follow.
  - :keyword")
  ;; This is not even remotely correct arguments passed, the point is
  ;; to verify that we send an error
  :test (expect-error
         (:documentation "Calling the custom reader function should automaticallythrow an error at this point. This is what I (nixeagle) term defaulting to sane defaults. Deny everything!")
         (ensure-condition 'simple-error
                        (colon-reader nil nil))))

(deftestsuite test-list-to-pair (root-suite)
  ()
  (:test (even-list
          (:documentation "Even lists should return two values, car and cdr.")
          (list-to-pair (list 1 2))
          (ensure-same (list-to-pair)
                       (values 1 2))))
  (:test (empty-list
          (:documentation "Empty lists return two nils")
          (ensure-same (list-to-pair)
                       (values nil nil))))
  (:test (odd-list
          (:documentation "Odd lists should error")
          (ensure-condition (simple-error)
            (list-to-pair (list 1))
            (list-to-pair)))))

(defpackage #:safe-external-tests
  (:use :lift
        :safe-external
        :safe-closure)
  (:documentation "This package is largely reserved for testing "))

;(use-package *prepared-safe-packages* (find-package :safe-external-tests))
(in-package :safe-external-tests)


(deftestsuite root-suite (nisp::root-suite)
  ()
  (:documentation "The root of all external (sandbox accessable) function tests."))

(deftestsuite safe-closure-suite (root-suite)
  ()
  (:documentation "Items in safe-closure have the requirement of
allowing operations on a safe-package without the user of the safe
package being allowed to know which package they are in." ))


(deftestsuite test-setq (safe-closure-suite)
  ()
  (:documentation "Verify that no form of setq permitted in a safe
package is allowed to modify symbols outside of the package."))


