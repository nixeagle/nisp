;;; The idea here is to do safe evaluation by reading using alternate
;;; packages

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

(defun format-package-name (name)
  "Take name and append *safe-package-prefix*

This is a cheap way to namespace packages. Better ideas welcome."
  (concatenate 'string *safe-package-prefix* name))

(test format-package-name
  "Result should be with *safe-package-prefix* prepended"
  (is (string= "safe-test" (format-package-name "test"))))

(defun make-empty-safe-package (name)
  (make-package (format-package-name name)))

(defun delete-safe-package (name)
  (delete-package (format-package-name name)))

(test create-empty-safe-package
  (is (packagep (make-empty-safe-package "safe-test1"))))

(test (delete-safe-package :depends-on create-empty-safe-package)
  (is-true (delete-safe-package "safe-test1")))