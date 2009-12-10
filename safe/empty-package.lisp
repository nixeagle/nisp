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

(in-package :nisp-empty-package)

(def-suite empty-packages
    :in nisp-safe::safe-suite
    :description "Tests related to empty packages")
(in-suite empty-packages)

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
     (progn
       (setq name (package-name *package*))
       (apply #'values
              (prog1 (multiple-value-list ,@body)
                (delete-package name))))))


;;;; Handle creating empty packages.


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