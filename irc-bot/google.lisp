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

(defun get-html-translate-page (text from to)
  "Get html page with the translation of TEXT FROM language TO language."
  (declare (type string text from to))
  (drakma:http-request "http://translate.google.com/"
                       :parameters `(("eotf" . "1")
                                     ("hl" . "en")
                                     ("ie" . "UTF-8")
                                     ("js" . "y")
                                     ("layout" . "1")
                                     ("prev" . "_t")
                                     ("sl" . ,from)
                                     ("text" . ,text)
                                     ("tl" . ,to))
                       :external-format-out :UTF-8
                       :keep-alive t
                       :close nil))

(defun xml->sexp (xml-string)
  "Convert XML-STRING to the lisp representation."
  (declare (type string xml-string))
  (chtml:parse xml-string (cxml-stp:make-builder)))

(defun get-normal-translate-data-node (node)
  "NODE's data field is returned.

try should be of a short_text html/css class."
  (declare (type stp:node node))
  (let ((data (car (stp:list-children (car (stp:list-children node))))))
    (unless data (error "No data node found, likely a language conversion failed to work. Make sure things like es -> jp actually produce a meaningful translation."))
    (stp:data data)))

(defun simple-translate (text from to)
  "Translate TEXT FROM a language TO another language."
  (declare (type string text from to))
  (xml->sexp (get-html-translate-page text from to)))

;;; end file, please leave trailing newline.
