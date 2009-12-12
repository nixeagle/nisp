;;; The idea here is to do safe evaluation by reading using alternate
;;; packages
;;;
;;; This is not _intended_ for use by anyone anywhere other then the author.
;;; the package is still very incomplete and untested.
;;;
;;; Please note this code is highly experimental, if it blows up the
;;; moon, its not my fault ;)

(defpackage #:nisp-safe
  (:use :common-lisp
        :lift
        :metatilities
        :nisp-empty-package
        :nisp-util)
  (:export #:with-safe-package
           #:with-safe-readtable
           ))

(in-package :nisp-safe)

(deftestsuite root-suite (nisp::root-suite) ())

(defvar *prepared-safe-packages*
  '(:safe-arithmetic
    :safe-arithmetic-trig
    :safe-arithmetic-comparision
    :safe-arithmetic-type-manipulation
    :safe-arithmetic-boole
    :safe-arithmetic-implentation-constants
    :safe-arithmetic-random
    :nisp-safe-introspect)
  "Listing of packages that have been prepared or deemed to be safe.
This is the default list, the idea is depending on the situation mix and
match what capacities you want to allow untrusted code to do.

On the todo list is to create a class that allows multiple instances and
tracks currently interned packages and whatnot.")

(defun make-safe-package (name)
   "Should make a totally independent package. All normal safe stuff should be in this by default.

Some questions to consider:
  - Is it safe to set *readtable* in the package on creation. By this is it possible for a malicious user to modify this?
    - What happens when I set it to nil?
    - Can I replace colon-reader with another more permissive function? If so how?
  - Is it safe to read using a read function defined in this package? My off the cuff guess is 'not a good idea'. Too much chance for iffy behavior unless it can be proven safe."
  (nyi name))

(deftestsuite make-safe-package (root-suite)
  ((keyword :test-keyword)
   (string "test-string"))
  :test (pass-keyword
         (ensure (packagep (make-safe-package keyword))))
  :test (pass-string
         (ensure (packagep (make-safe-package string)))))

(defun delete-safe-package (name)
  "Delete package NAME unless its already deleted."
  (when (packagep (find-package name))
    (delete-package name)))

(defun read-using-package (name string)
  "read STRING using package NAME."
  (let ((*package* (find-package name)))
    (read-from-string string)))

(defun colon-reader (stream char)
  "Signal an error if a colon is used to access another package."
  (declare (ignore stream char))
  ;; For now we just error out. This is a safe thing to do as far as
  ;; disabling package access goes, but in the process use of : is
  ;; broken for :keywords and possibly other things too.
  (error "Accessing packages outside of the current one is disabled."))

(deftestsuite base-packages (root-suite)
  ((base-empty-packages (list '"test1" '"test2" '"test3")))
  (:setup
   (mapc #'make-empty-package base-empty-packages))
  (:teardown
   (mapc #'delete-safe-package base-empty-packages)))

(deftestsuite test-colon-reader (base-packages)
  ()
  ;; This is not even remotely correct arguments passed, the point is
  ;; to verify that we send an error
  :test (expect-error
         (ensure-condition 'simple-error
                        (colon-reader nil nil))))

(defun make-readtable ()
  "Create readtable that prevents any syntax that can cause a package
  change."
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\: #'colon-reader nil *readtable*)
    *readtable*))

(deftestsuite test-make-readtable (root-suite)
  ()
  :test (colon-macro-function-bound?
         (ensure (functionp (get-macro-character #\: (make-readtable))))))

(defmacro with-safe-readtable (&body body)
  "Use readtable for all read calls in body.If readtable is not passed,
we default to instantiating a new one using make-readtable"
  `(let ((*readtable* *safe-readtable*))
     ,@body))

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

;;; Moving this down here for now after make-readtable is defined
;;; because this depends on that function being defined.
(defparameter *safe-readtable* (make-readtable)
  "The safe readtable that is used by the with-safe-readtable macro.

This is currently a parameter but should be made into a constant at some
point so a warning or error can block it being changed in a running
program.")

(deftestsuite test-*safe-readtable* (root-suite)
    ()
    :documentation "Make sure we get a readtable."
    :test (is-readtable
           (ensure (readtablep *safe-readtable*))))

(defmacro with-safe-package (name &body body)
  "Use the given package name to read some code."
  `(with-package ,name
     (with-safe-readtable
       ,@body)))

