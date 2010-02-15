(in-package :nisp.i-zeta)

;;; These need to be split out to make sure that the funcallable class
;;; is compiled first.

(defgeneric test-gf (a b c)
  (:generic-function-class test-generic-function))
(defmethod test-gf ((a integer) (b integer) (c integer))
  (list (this-method)
        (call-next-method)))
(defmethod test-gf ((a string) (b integer) (c integer))
  (+ b c))
(defmethod test-gf ((a string) (b string) (c string))
  (concatenate 'string a b c))
(defmethod test-gf (a b c)
  (this-method))

(defgeneric demo-this-method ()
  (:generic-function-class test-generic-function))
(defmethod demo-this-method ()
  (this-method))