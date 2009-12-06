;;; Introspection of lisp systems. May be portable one day.
(in-package :nisp-introspect)


(defgeneric function-lambda-list (symbol)
  (:method (symbol)
    (values
     (format nil "Passed input ~A not a valid extended function designator."
             (type-of symbol))
     'nil))
  (:documentation
   "Return lambda list description given a string or a valid function
   designator."))

(defmethod function-lambda-list ((symbol function))
  (values
   #+sbcl (sb-introspect:function-lambda-list symbol)
   #-sbcl (error)
   't))

(defmethod function-lambda-list ((symbol string))
  (function-lambda-list (intern symbol)))

(defmethod function-lambda-list ((symbol symbol))
  (if (fboundp symbol)
      (function-lambda-list (symbol-function symbol))
      (function-lambda-list t)))