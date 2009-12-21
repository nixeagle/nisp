;;; Introspection of lisp systems. May be portable one day.
(in-package :nisp-introspect)

(deftestsuite root-suite () ())

(defgeneric function-lambda-list (symbol)
  (:documentation
   "Return lambda list description given a string or a valid function designator."))

(defmethod function-lambda-list :around (symbol)
  "Handle errors originating from a function-lambda list call."
  (handler-case (call-next-method)
        (error ()
          (format nil "Passed input type ~A not valid extended function designator."
                  (type-of symbol)))))

(defmethod function-lambda-list (symbol)
    #+sbcl (sb-introspect:function-lambda-list symbol))



(defmethod function-lambda-list ((symbol string))
  "If we get a string, read it and re-call."
  (function-lambda-list (read-from-string symbol)))


(deftestsuite test-function-lambda-list (root-suite)
  ((plus-arg-list '(&REST SB-KERNEL::ARGS)))
  :test (pass-valid-symbol
         (ensure-same (function-lambda-list '+)
                      plus-arg-list))
  :test (pass-invalid-symbol
         (ensure (stringp (function-lambda-list 1))))
  :test (pass-valid-string
         (ensure-same (function-lambda-list "+")
                      plus-arg-list)))