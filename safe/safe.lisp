;;; The idea here is to do safe evaluation by reading using alternate
;;; packages
;;;
;;; This is not _intended_ for use by anyone anywhere other then the author.
;;; the package is still very incomplete and untested.
;;;
;;; Please note this code is highly experimental, if it blows up the
;;; moon, its not my fault ;)

(in-package :nisp-safe)

(deftestsuite root-suite (nisp::root-suite) ())

(defvar *safe-package-prefix* "safe-"
  "All packages created for holding known safe code are stored
with this package prefix.")

(deftestsuite test-*safe-variable-prefix* (root-suite)
  ()
  :test (is-string (ensure (stringp *safe-package-prefix*)))
  :test (last-element
         (:documentation "Last element of the prefix needs to be a dash")
         (ensure-same (car (last (coerce *safe-package-prefix* 'list)))
                      #\-)))

(defun format-package-name (name)
  "Take name and append *safe-package-prefix*

This is a cheap way to namespace packages. Better ideas welcome."
  (concatenate 'string *safe-package-prefix* name))

(deftestsuite test-format-package-name (root-suite)
  ()
  :test (result-has-prefix
         (ensure-same (format-package-name "test")
                      "safe-test")))

(defun make-empty-safe-package (name)
  "Make a package prefixed with the safe prefix specified in
*safe-package-prefix*"
  (make-empty-package (format-package-name name)))

(deftestsuite test-make-empty-safe-package (root-suite)
  ()
  (:teardown (when (find-package "safe-test1")
               (delete-package "safe-test1")))
  :test (is-package
         (ensure (packagep (make-empty-safe-package "test1")))))

  ;; :test (pass-empty-string
  ;;        (:documentation "We should error if an empty string is passed")
  ;;        (ensure-error (make-empty-safe-package "")))

(defun delete-safe-package (name)
  "Delete package NAME unless its already deleted."
  (let ((safe-package-name (format-package-name name)))
    (when (packagep (find-package safe-package-name))
      (delete-package safe-package-name))))

;; (test (delete-safe-package :depends-on make-empty-safe-package)
;;   (is-true (delete-safe-package "test1")))

;;;; base package stuff
;;;do we really need this? -- nixeagle 2009-12-10

(nisp-util::define-constant  +base-empty-packages+
    '("test1" "test2" "test3")
  "Base safe packages as far as development goes.")

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
  ()
  (:setup
   (mapc #'make-empty-safe-package +base-empty-packages+))
  (:teardown
   (mapc #'delete-safe-package +base-empty-packages+)))

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
  ;((*readtable* (make-readtable)))
  ()
  :test (colon-macro-function-bound?
         ;(:documentation
         ; "Created read table should have a function bound to #\\\:")
         (let ((*readtable* (make-readtable)))
           (ensure (functionp (get-macro-character #\:))))))

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
              (read-using-package "safe-test1" "cl::+")))))))

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
  `(with-package ,(format-package-name name)
     (with-safe-readtable
       ,@body)))

