;;; Introspection of lisp systems. May be portable one day.
(in-package :nisp-introspect)


(defgeneric function-lambda-list (symbol)
  (:method
      :around (symbol)
      (handler-case
          (call-next-method)
        (error () (format nil "Passed input type ~A not valid extended function designator." (type-of symbol)))))

  
  (:documentation
   "Return lambda list description given a string or a valid function
   designator."))

(defmethod function-lambda-list ((symbol function))
  
  #+sbcl (sb-introspect:function-lambda-list symbol)
  #-sbcl (error)
  )

(defmethod function-lambda-list ((symbol string))
  (function-lambda-list (intern symbol)))

(defmethod function-lambda-list ((symbol symbol))
  (if (fboundp symbol)
      (function-lambda-list (symbol-function symbol))
      (error 'error symbol)))

