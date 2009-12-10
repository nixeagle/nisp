;;; The idea here is to do safe evaluation by reading using alternate
;;; packages
;;;
;;; This is not _intended_ for use by anyone anywhere other then the author.
;;; the package is still very incomplete and untested.
;;;
;;; Please note this code is highly experimental, if it blows up the
;;; moon, its not my fault ;)

(in-package :nisp-safe)
(in-suite safe-suite)

(defvar *safe-package-prefix* "safe-"
  "All packages created for holding known safe code are stored
with this package prefix.")

(test *safe-package-prefix*
  "Should be a string with a trailing -"
  (is (stringp *safe-package-prefix*))
  (is (eql #\-
           (car (last (coerce *safe-package-prefix* 'list))))))

(nisp-util::define-constant  +base-empty-packages+
    '("alpha" "beta" "general")
  "Base safe packages as far as development goes.")

(defun format-package-name (name)
  "Take name and append *safe-package-prefix*

This is a cheap way to namespace packages. Better ideas welcome."
  (concatenate 'string *safe-package-prefix* name))

(test format-package-name
  "Result should be with *safe-package-prefix* prepended"
  (is (string= "safe-test" (format-package-name "test"))))

(defun make-empty-safe-package (name)
  "Make a package prefixed with the safe prefix specified in
*safe-package-prefix*"
  (make-empty-package (format-package-name name)))

(test make-empty-safe-package
  (is (packagep (make-empty-safe-package "test1"))))

(defun delete-safe-package (name)
  "Delete package NAME unless its already deleted."
  (let ((safe-package-name (format-package-name name)))
    (when (packagep (find-package safe-package-name))
      (delete-package safe-package-name))))

(test (delete-safe-package :depends-on make-empty-safe-package)
  (is-true (delete-safe-package "test1")))

(defun make-empty-base-packages ()
  "Using the packages named in +base-empty-packages+ create a set of
packages that are empty for development experimentation."
  (mapc #'make-empty-safe-package +base-empty-packages+))

(defun delete-base-packages ()
  "Undo the actions of make-empty-base-packages."
  (mapc #'delete-safe-package +base-empty-packages+))

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

(test colon-reader
  (signals (simple-error)
    ;; This is not even remotely correct arguments passed, the point is
    ;; to verify that we send an error
    (colon-reader nil nil)))

(defun make-readtable ()
  "Create readtable that prevents any syntax that can cause a package
  change."
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\: #'colon-reader nil *readtable*)
    *readtable*))

(test make-readtable
  (let ((*readtable* (make-readtable)))
    (is (functionp (get-macro-character #\:))
        "Created read table should have a function bound to #\\\:")))

(5am:def-fixture base-package-fixture ()
  (delete-base-packages)
  (make-empty-base-packages)
  (prog1
      (let ((*readtable* (make-readtable)))
        (&body))
    (delete-base-packages)))

(test (safe-read-with-colons :fixture base-package-fixture)
  (is (not (fboundp (read-using-package "safe-alpha" "cl::+")))
      "IF YOU SEE THIS:: DO NOT EVEN THINK ABOUT IGNORING IT!

This message means that a function outside of a socalled 'safe' package
was accessed. This test does the following:

1) create an empty package, eg one with no internal symbols
2) runs read-from-string using the package as the *PACKAGE* variable.
3) reads the following string \"cl::+\".
4) If cl::+ is an fbound symbol a critical assumption failed
   or a regression has occured.

AGAIN DO NOT EVEN THINK ABOUT USING WHILE THIS TEST FAILS!"))

(defmacro with-safe-readtable (&body body)
  "Use readtable for all read calls in body.If readtable is not passed,
we default to instantiating a new one using make-readtable"
  `(let ((*readtable* *safe-readtable*))
     ,@body))

(test (with-safe-readtable :depends-on *safe-readtable*)
  (with-safe-readtable
    (is (eq *readtable* *safe-readtable*)
        "Make sure that inside this form *readtable* is set to *safe-readtable*")))

;;; Moving this down here for now after make-readtable is defined
;;; because this depends on that function being defined.
(defparameter *safe-readtable* (make-readtable)
  "The safe readtable that is used by the with-safe-readtable macro.

This is currently a parameter but should be made into a constant at some
point so a warning or error can block it being changed in a running
program.")

(test (*safe-readtable* :depends-on (and make-readtable))
  "Make sure we get a readtable."
  (is (readtablep *safe-readtable*)
      "Should be a readtable, nothing else"))

(defmacro with-safe-package (name &body body)
  `(with-package ,(format-package-name name)
     (with-safe-readtable
       ,@body)))

(test (with-safe-package :depends-on (and
                                      with-safe-readtable
                                      with-package))
  )

