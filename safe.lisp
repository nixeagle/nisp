;;; The idea here is to do safe evaluation by reading using alternate
;;; packages
;;;
;;; This is not _intended_ for use by anyone anywhere other then the author.
;;; the package is still very incomplete and untested.
;;;
;;; Please note this code is highly experimental, if it blows up the
;;; moon, its not my fault ;)

(in-package :nisp-safe)
(5am:def-suite safe-suite
    :in nisp::all-tests)
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
  (make-package (format-package-name name) :use 'nil))

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

(defmacro with-package (package &body body)
  "Evaluate body in the context of package-name."
  `(let ((*package* (find-package ,package)))
     ,@body))

(test with-package
  ;; We have repeated with package stuff, this should be done better but
  ;; the point is to verify that both ASDF and :asdf work. We know that
  ;; "asdf" won't work because that implies |asdf| (case sensitivity).
  (is (eq #'asdf:oos
          (with-package "ASDF"
            (symbol-function (read-from-string "oos"))))
      "Needs to return the symbol internal to asdf")
  (is (eq #'asdf:oos
          (with-package :asdf
            (symbol-function (read-from-string "oos"))))
      "Needs to return the symbol internal to safe-alpha"))

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


;;;; General purpose interface to making new packages
;;;The basic idea with this is to make a package that has nothing in it,
;;;and to be able to make these packages without specifying a name every
;;;time we make one. This property is useful for tests as well as for
;;;making a new empty package on user request. Some examples of use
;;;include giving each 'user' their own namespace so experiments by one
;;;user will not cause clashes by experiments of the other.
(defun make-empty-package (name)
  "Create an empty package called NAME.

This is just a wrapper around (make-package) with extra arguments to
ensure the generated package really is empty on all implentations. We
have to do this because the ansi standard leaves what to do with
MAKE-PACKAGE with no arguments implentation defined."
  (make-package name :use 'nil))


;;;; Generate an empty package.
;;; Please note this used to be a closure, but it is now defined as a
;;; global internal variable as the closure gets reset when the lisp
;;; file gets recompiled. Ideas or advice on how to have a closure set
;;; itself only once and never reset are welcome.
(defvar *empty-package-counter* 0
  "Internal global variable to increment the ids instead of using a
  closure as a closure gets reset whenever a file gets re-compiled.")

(defun gen-empty-package (&optional prefix)
    "Generate and return a package with a unique name prefixed by an integer

If prefix is not supplied, default to G-SAFE-"
    (let ((prefix (or prefix "G-SAFE-")))
      (make-empty-package
       (concatenate 'string
                    prefix (write-to-string (incf *empty-package-counter*))))))

(test (gen-empty-package :depends-on (and make-empty-package))
  "Generating an empty package should always give us a new package. If
at any time we are able to generate a package collision a bug has been
found."
  ;; test fails, not implemented
  (let ((package1 (gen-empty-package))
        (package2 (gen-empty-package)))
    (is (not (eq package1 package2))
        "Should not get the same object from two different calls to
gen-empty-package")))

;;; I've chosen to name this 'with-empty-package' and not
;;; 'with-temp-empty-package' as it is implied that the package is
;;; temporary given there are no other arguments to the macro other then
;;; the body forms. There is no real good way for passed body forms to
;;; get the name of the package other then to wrap the macro around in a
;;; let form or some other method of passing the package name from
;;; inside the macro outside of it.
(defmacro with-empty-package (&body body)
  "Run body with *package* set to an empty package and remove the
package after the body is done executing.

This is mostly motivated for use in test cases."
  `(let* ((name (gensym))
          (*package* (gen-empty-package)))
     (block block-name
       (progn
         (setq name (package-name *package*))
         (prog1 ,@body
           (delete-package name))))))

(def-suite empty-packages
    :in safe-suite
    :description "Tests related to empty packages")
(in-suite empty-packages)

(test make-empty-package
  "Package returned should have nothing in it"
  ;; package name is intentionally long and mixed case. Doing so means
  ;; no chance of colliding with some other package.
  (let ((empty-name "safe-SoMe-Long-paKAGeName"))
    (when (packagep (find-package empty-name))
      (delete-package empty-name))
    (let ((empty-package (make-empty-package empty-name)))
      (is (packagep empty-package)
          "A package is expected, even if its not empty. (base assumption)")
      (is (= 0 (nisp-util::count-symbols empty-name))
          "A package is not empty if it has more then 0 elements."))))

;;; Right now this test fails to detect the condition it is looking
;;; for. Right now I do not see any portable way to verify that a
;;; package has actually been deleted
(test (is-deleted :depends-on (and gen-empty-package))
  "Returned (deleted) package will be nil according to packagep. If
this returns an undeleted function then we did not do the cleanup work
properly"
  (is (not (packagep
            (with-empty-package *package*)))))

(test (with-empty-package :depends-on (and gen-empty-package))
  "Expect 0 symbols in an empty package."
  (with-empty-package
    (is (= 0 (nisp-util::count-symbols)))))

(test (multiple-return-values :depends-on (and gen-empty-package))
  "Calling with empty package should not change what is returned from
the function. All return values need to be preserved."
  (is (equal '(1 a)
             (multiple-value-list
              (with-empty-package
                (values '1 'a))))))

(test (single-return-value :depends-on (and gen-empty-package))
  "We don't want to break the simple cases. If a function returns 3, we
expect 3 no more and no less. (3) or an error is not acceptable."
  (is (= 3 (with-empty-package
             3))))