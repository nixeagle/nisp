(in-package :nisp.mop-simple)

(macrolet ((define-mop (name)
             `(progn
                (defgeneric ,name (class))
                (defmethod ,name ((class class))
                   (,(alexandria:ensure-symbol name :closer-mop) class))
                (defmethod ,name ((instance standard-object))
                   (,name (class-of instance)))
                (defmethod ,name ((class symbol))
                   (,name (find-class class)))
                (defmethod ,name ((class string))
                   (,name (find-symbol class))))))
  (define-mop class-default-initargs)
  (define-mop class-direct-default-initargs)
  (define-mop class-slots)
  (define-mop class-direct-slots)
  (define-mop class-direct-subclasses)
  (define-mop class-direct-superclasses)
  (define-mop class-finalized-p)
  (define-mop class-precedence-list)
  (define-mop class-prototype)
  (define-mop compute-class-precedence-list)
  (define-mop compute-default-initargs)
  (define-mop compute-slots)
  (define-mop finalize-inheritance))


;;; End