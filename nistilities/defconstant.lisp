;; Advice given by the SBCL manual on how to properly use defconstant.
;; Source taken from http://www.sbcl.org/manual/Defining-Constants.html
;;
;;;; Changelog
;; 2009-12-08 - add doc string
(in-package :nistilities)

(defmacro define-constant (name value &optional doc)
  "Define a truely constant constant that is never modified once it is evaluated."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
