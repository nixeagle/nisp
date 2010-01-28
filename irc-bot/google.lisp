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
  "The translation in NODE's data field is returned.

NODE should be of a short_text html/css class."
  (declare (type stp:node node))
  (if (typep node 'stp:text)
      (stp:data node)
      (get-normal-translate-data-node (car (stp:list-children node)))))

(defun get-suggestion-translate-data-node (node)
  "The translation in NODE's data field is returned."
  (declare (type (and stp:node (satisfies translate-suggestion-p)) node))
  (stp:data (car (stp:list-children node))))

(defun simple-translate (text from to)
  "Translate TEXT FROM a language TO another language."
  (declare (type string text from to))
  (or (stp:do-recursively (a (xml->sexp (get-html-translate-page text from to)))
        (cond
          ((translate-short-text-p a)
           (return (get-normal-translate-data-node a)))
          ((translate-suggestion-p a)
           (return (get-suggestion-translate-data-node a)))))
      (error "Translation from ~A to ~A using text \"~A\" has no valid result."
             from to text)))

(defun google-translate (text &rest langs)
  "Translate TEXT apllying the result in succession for each language in LANGS."
  (let ((fresh-langs langs))
    (iter (with first = (pop fresh-langs))
          (with result = text)
          (for lang in fresh-langs)
          (setq result (simple-translate result first lang)
                first lang)
          (finally (return result)))))


;;;;; Search


;;; Taken from http://common-lisp.net/project/closure/closure-html/examples.html
;;; and modified to do what I need.
;;;
;;; stopping where I need to finish the m-v-b and somehow return the
;;; google query along with the string of the google results.
;      result (puri:uri-query uri)
(defun google-search (term)
  "Google TERM and return the top 10 links."
  ;; This is kinda old (from nisp.irc.google-oneline-list) and needs
  ;; rewriting using the utilities developed in this file.
  (multiple-value-bind (result response alist uri)
      (drakma:http-request "http://www.google.com/search"
                           :parameters (list (cons "q" term)))
      (declare (ignore response alist))
      (let ((document (chtml:parse result (cxml-stp:make-builder)))
            (out ""))
        (stp:do-recursively (a document)
          (when (and (typep a 'stp:element)
                     (equal (stp:local-name a) "a")
                     (equal (stp:attribute-value a "class") "l"))
            (setq out (format nil "~A ~A" out (stp:attribute-value a "href")))))
        (format nil "Search: ~A ::: ~A" uri out))))


;;; end file, please leave trailing newline.
