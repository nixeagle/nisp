(in-package :nisp-safe)

;; Tests
(deftestsuite base-packages (root-suite)
  ((base-empty-packages '"test1"))
  (:setup
   (mapc #'make-empty-package base-empty-packages))
  (:teardown
   (mapc #'delete-safe-package-old base-empty-packages)))

(deftestsuite root-suite (nisp::root-suite) ())

(deftestsuite test-make-readtable (root-suite)
  ()
  :test (colon-macro-function-bound?
         (ensure (functionp (get-macro-character #\: (make-readtable))))))

(deftestsuite test-with-safe-readtable (base-packages)
  ()
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
  (:teardown (delete-package :test-keyword)
             (delete-package "test-string"))
  :test (pass-keyword
         (ensure (packagep (make-safe-package keyword))))
  :test (pass-string
         (ensure (packagep (make-safe-package string)))))


(deftestsuite test-colon-reader (base-packages)
  ()
  ;; This is not even remotely correct arguments passed, the point is
  ;; to verify that we send an error
  :test (expect-error
         (ensure-condition 'simple-error
                        (colon-reader nil nil))))

(defpackage #:safe-external-tests
  (:use :lift)
  (:documentation "This package is largely reserved for testing "))

(use-package *prepared-safe-packages* (find-package :safe-external-tests))
(in-package :safe-external-tests)


(deftestsuite root-suite (nisp::root-suite)
  ()
  (:documentation "The root of all external (sandbox accessable) function tests."))

(deftestsuite test-range (root-suite)
  ()
  (:function
   (run-range (start end)
              (ensure (cl:listp (range start end)))
              (ensure (cl:< 0 (cl:list-length (range start end))))))
  (:test (count-up
          (:documentation "The usual case, counting from START to END")
          (ensure-same (range 0 10)
                       (list 0 1 2 3 4 5 6 7 8 9 10))))
  (:test (count-down
          (:documentation
           "A tad unusual, but we expect given a START greater then END
to count _down_ from START to END.")
          (ensure-same (range 5 -5)
                       (list 5 4 3 2 1 0 -1 -2 -3 -4 -5))))
  (:test (random-integer-combos
          (:documentation "Playing around with random test cases. These
are largely undocumented in the lift test framework.")
          (lift:ensure-random-cases 10
              ((start an-integer)
               (end an-integer))
            (run-range start end))))
  (:test (random-float-combos
          (:documentation "Make sure that no combination of floats break range.")
          (lift:ensure-random-cases 10
              ((start a-double-float)
               (end a-single-float))
            (run-range start end)))))