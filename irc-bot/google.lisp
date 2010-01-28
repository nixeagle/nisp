(defpackage #:nisp.google
  (:use :cl :iterate)
  (:export #:google-search
           #:google-translate))
(in-package :nisp.google)

(defun elementp (node)
  "True if NODE represents a `stp:element'"
  (declare (type stp:node node))
  (typep node 'stp:element))

(defun translate-short-text-p (node)
  "True if NODE has the translation text.

Note that the translation text is stored in a child node of NODE."
  (declare (type stp:node node))
  (and (elementp node)
       (equal (stp:attribute-value node "class") "short_text")))

(defun translate-suggestion-p (node)
  (declare (type stp:node node))
  (and (elementp node)
       (equal (stp:attribute-value node "id") "suggestion")))