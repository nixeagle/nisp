;;; The idea here is to do safe evaluation by reading using alternate
;;; packages

(in-package :nisp-safe)
(in-suite safe-suite)

(defvar *safe-package-prefix* "safe-"
  "All packages created for holding known safe code are stored
with this package prefix.")


(defun format-package-name (name)
  "Take name and append a prefix.

This is a cheap way to namespace packages. Better ideas welcome."
  (concatenate 'string *safe-package-prefix* name))

(defun make-empty-safe-package (name)
  (make-package (format-package-name name)))

(defun delete-safe-package (name)
  (delete-package (format-package-name name)))

(test create-empty-safe-package
  (is (packagep (make-empty-safe-package "safe-test1"))))

(test (delete-safe-package :depends-on create-empty-safe-package)
  (is-true (delete-safe-package "safe-test1")))