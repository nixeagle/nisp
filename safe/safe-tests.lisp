(in-package :nisp-safe)

;; Tests
(deftestsuite base-packages (root-suite)
  ()
  (:setup
   (make-empty-package "test1"))
  (:teardown
   (delete-safe-package-old "test1")))

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
  (:teardown (delete-safe-package-old keyword)
             (delete-safe-package-old string))
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

(deftestsuite test-range (root-suite)
  ()
  (:documentation
   "Perform tests on range convenience function. In general it should handle any useful range.")
  (:function
   (run-range (start end)
              (ensure (cl:listp (range start end)))
              (ensure (cl:< 0 (cl:list-length (range start end))))))
  (:test (count-up
          (:documentation "The usual case, counting from START to END")
          (ensure-same (range 0 10)
                       (cl:list 0 1 2 3 4 5 6 7 8 9 10))))
  (:test (count-down
          (:documentation
           "A tad unusual, but we expect given a START greater then END
to count _down_ from START to END.")
          (ensure-same (range 5 -5)
                       (cl:list 5 4 3 2 1 0 -1 -2 -3 -4 -5))))
  (:test (random-integer-combos
          (:documentation
           "Playing around with random test cases. These are largely
undocumented in the lift test framework.")
          (lift:ensure-random-cases 10
              ((start an-integer)
               (end an-integer))
            (run-range start end))))
  (:test (random-float-combos
          (:documentation
           "Make sure that no combination of floats break range.")
          (lift:ensure-random-cases 10
              ((start a-double-float)
               (end a-single-float))
            (run-range start end))))
  (:test (count-up-with-char
          (:documentation "Count up from a lower START char to a higher
END char.")
          (ensure-same (range #\a #\c)
                       (list #\a #\b #\c))))
  (:test (count-down-from-char
          (:documentation "Count down from a higher START to a lower END
char.")
          (ensure-same (range #\c #\a)
                       (list #\c #\b #\a)))))

(deftestsuite test-setq (safe-closure-suite)
  ()
  (:documentation "Verify that no form of setq permitted in a safe
package is allowed to modify symbols outside of the package.")
 #+ () (:test (change-help-variable-locally
          (:documentation "help is an exported symbol, we need to be able to make changes without modifying the external package.")
          (ensure ))))

