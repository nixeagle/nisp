;;;; Handle the creation, and deletion of empty packages.
;;; Contained here is lisp that creates a totally empty package with
;;; nothing in it by a given name, or by a unique name. Additionally a
;;; few macros are exported to handle the basic use cases with clean
;;; syntax.
;;;
;;; This by itself does not ensure safety of foreign code
;;; evaluation. The major point of empty packages are so that the lisp
;;; reader sees no interned symbols but what is put in there by the
;;; application programmer. Reading from inside an empty package alone
;;; does not ensure saftey. You can access symbols outside of an empty
;;; package with cl::+ syntax.
;;;
;;; The basic idea with this is to make a package that has nothing in it,
;;; and to be able to make these packages without specifying a name every
;;; time we make one. This property is useful for tests as well as for
;;; making a new empty package on user request. Some examples of use
;;; include giving each 'user' their own namespace so experiments by one
;;; user will not cause clashes by experiments of the other.

(defpackage #:nisp-empty-package
  (:use :cl :lift :nisp-util)
  (:export #:with-empty-package
           #:make-empty-package
           #:with-package
           #:gen-empty-package
           ))

(in-package :nisp-empty-package)

(deftestsuite root-suite (nisp::root-suite) ())

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

(deftestsuite test-gen-empty-package (root-suite)
  ()
  (:test (generate-different-packages
          (:documentation "
Should not get the same object from two
different calls to gen-empty-package
")
          (ensure-different (gen-empty-package)
                       (gen-empty-package))))
  (:documentation "Generating an empty package gives us a new
package. If at any time we are able to generate a package collision a
bug has been found."))

#+ (or)
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
     (progn
       (setq name (package-name *package*))
       (apply #'values
              (prog1 (multiple-value-list ,@body)
                (delete-package name))))))


;;;; Handle creating empty packages.

(deftestsuite test-make-empty-package (root-suite)
  ((empty-name "safe-SoMe-Long-paKAGeName")
   (empty-package))
  (:setup (setq empty-package (make-empty-package empty-name)))
  (:teardown (delete-package empty-name))
  :test (expect-package
         (ensure (packagep empty-package)))
  :test (expect-zero-elements
         (ensure-same (nisp-util::count-symbols empty-name) 0)))

(deftestsuite test-with-empty-package (root-suite)
  ()
  ;; commenting this one out for now, its giving a spurious test
  ;; failure. The package when deleted is still a package.
  #+ (or) (:test
           (deleted-package
            (:documentation "Returned (deleted) package will be nil
according to packagep. If this returns an undeleted function then we did
not do the cleanup work properly")
            (ensure (not (packagep (with-empty-package *package*))))))
  :test (package-has-no-symbols
         (ensure (with-empty-package
                   (= 0 (nisp-util::count-symbols)))))
  :test (multiple-return-values
         (ensure-same 
          (multiple-value-list
           (with-empty-package
             (values '1 'a))) '(1 a)))
  :test (single-return-value
         (ensure-same (with-empty-package 3) 3)))

(defmacro with-package (package &body body)
  "Evaluate body in the context of package-name."
  `(let ((*package* (find-package ,package)))
     ,@body))

(deftestsuite test-with-package (root-suite)
  ()
  :test (pass-string
         (ensure-same
          (with-package "ASDF"
            (package-name *package*))
          "ASDF"))
  :test (pass-keyword
          (ensure-same
           (with-package :asdf
             (package-name *package*))
           "ASDF"))
  :documentation "Getting the package name of *package* should be
  changed to the package specified in the call to with-package")