
(in-package #:nisp-safe)

;; Tests
(deftestsuite root-suite ()
  ()
  (:categories))
(deftestsuite base-packages (root-suite)
  ()
  (:setup
   (make-empty-package "test1"))
  (:teardown
   (delete-safe-package-old "test1")))

(deftestsuite test-make-readtable (root-suite)
  ()
  (:documentation
   "Make sure that a new read table also comes with #\\\: function bound.")
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
  ()
  (:teardown (delete-safe-package-old :test-keyword)
             (delete-safe-package-old "test-string"))
  :test (pass-keyword
         (ensure (packagep (make-safe-package :test-keyword))))
  :test (pass-string
         (ensure (packagep (make-safe-package "test-string")))))


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

(deftestsuite safe-package-suite ()
  ()
  (:documentation "Tests for correct safe package objects."))

(deftestsuite test-*prepared-safe-packages* (safe-package-suite)
  ()
  (:documentation "Prepared safe packages should be a list of keywords
mapping to *existing* packages")
  (:function
   (keyword-package-p (possible-package)
                      (ensure (packagep (find-package possible-package))
                              :report "Keyword ~S does not refer to a package"
                              :arguments (possible-package))))
  (:test (is-a-list
          (:documentation "If this is not a list, we are out of luck.")
          (ensure (listp *prepared-safe-packages*))))
  (:test (list-of-keywords
          (:documentation "all values in the list should be :keywords")
          (ensure-same (member-if-not #'keywordp *prepared-safe-packages*)
                       nil)))
  (:test (list-of-packages
          (:documentation "All the items in the list should refer to a package. Please note that we expect to return nil, on failure we will return the erroring element.")
          (mapc (lambda (x) (keyword-package-p x))
                *prepared-safe-packages*))))


;; (deftestsuite safe-package-fixture (safe-package-suite)
;;   ((~package~))
;;   (:setup (setq ~package~ (create-safe-package  "SAFE-PACKAGE-TEST-PACKAGE"
;;                                                "SAFE-PACKAGE-TEST-OWNER")))
;;   (:teardown (delete-safe-package  "SAFE-PACKAGE-TEST-PACKAGE")))

;; (defmacro with-safe-package-fixture (&body body)
;;   `(let ((~package~ (create-safe-package  "SAFE-PACKAGE-TEST-PACKAGE"
;;                                           "SAFE-PACKAGE-TEST-OWNER")))
;;      (prog1
;;          (progn
;;            ,@body)
;;        '(delete-package :safe-package-test-package))))

;; (deftestsuite test-package-use-from (safe-package-suite)
;;   ()
;;   (:documentation "Verify that different parameters to import do not
;; cause issues and that the imports do what they are supposed to
;; do. Nothing should be shadowed by default.")
;;   (:test
;;    (pass-string-and-keyword
;;     (with-safe-package-fixture
;;       (ensure (package-use-from "SAFE-PACKAGE-TEST-PACKAGE" :nisp)))))
;;   (:test
;;    (pass-keyword-and-keyword
;;     (with-safe-package-fixture
;;       (ensure (package-use-from :safe-package-test-package :nisp)))))
;;   (:test
;;    (pass-keyword-and-string
;;     (with-safe-package-fixture
;;       (ensure (package-use-from :safe-package-test-package "NISP")))))
;;   (:tests
;;    (pass-package
;;     (with-safe-package-fixture
;;       (ensure (package-use-from (find-package
;;                                  :safe-package-test-package) "NISP"))))
;;    (pass-safe-package
;;     (with-safe-package-fixture
;;       (ensure (package-use-from ~package~ "NISP"))))))
  
;; (deftestsuite test-get-package (safe-package-suite)
;;   ()
;;   (:documentation "Test that getting a package works as expected for all types.")
;;   (:function
;;    (ensure-packagep (x)
                    
;;                     (ensure (with-safe-package-fixture (packagep (get-package x)))
;;                             :report "Input ~A is not a package object."
;;                             :arguments (x))))
;;   (:tests
;;    (pass-string
    
;;     (ensure-packagep "SAFE-PACKAGE-TEST-PACKAGE"))
;;    (pass-keyword
    
;;     (ensure-packagep :safe-package-test-package))
;;    (pass-package
    
;;     (ensure-packagep (find-package :safe-package-test-package)))))

;; (deftestsuite test-shadowing-intern ()
;;   ()
;;   (:documentation "Verify that interning a symbol works as expected.")
;;   (:test
;;    (intern-in-safe
;;     (ensure-same (with-safe-package-fixture (shadowing-intern ~package~ '+))
;;                  nil)    ) ))

(defpackage #:safe-external-tests
  (:use :lift
        :safe-external
        :safe-closure)
  (:documentation "This package is largely reserved for testing "))

;(use-package *prepared-safe-packages* (find-package :safe-external-tests))

(in-package :safe-external-tests)


(deftestsuite root-suite ()
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