;;; Introspection of lisp systems. May be portable one day.
(in-package :nisp-introspect)

(def-suite nisp-introspect-suite
    :in nisp::all-tests)
(in-suite nisp-introspect-suite)

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

(test function-lambda-list/symbol
  "Anything other then a functionp input should return error string."
  (is (equal '(&REST SB-KERNEL::ARGS)
             (function-lambda-list '+)))
  (is (stringp (function-lambda-list 1))))



(defmethod function-lambda-list ((symbol string))
  "If we get a string, read it and re-call."
  (function-lambda-list (read-from-string symbol)))

(test function-lambda-list/string
  "Should return a lambda list of the function's arguments"
  (is (equal '(&REST SB-KERNEL::ARGS)
             (function-lambda-list "+"))
      "+ has always worked, but this is the base case."))
