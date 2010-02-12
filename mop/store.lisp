(in-package :nisp.mop)

(defun class-slot-name-value-alist (instance)
  "Return slot names and values of INSTANCE.

The alist looks something like: ((slot-name . slot-value) ...)."
  (mapcar (lambda (slot)
            (cons (closer-mop:slot-definition-name slot)
                  (closer-mop:slot-value-using-class (class-of instance) 
                                                     instance
                                                     slot)))
          (class-slots instance)))

(in-package :nisp.mop-store)

(defun generate-slot-specifier (slot)
  "Generate a slot-option form for SLOT.

This form is valid for use in `defclass'."
    (labels
        ((format-keyword (keyword function &key bad-result)
           (let ((result (funcall function slot)))
             (and (not (eq bad-result result)) `(,keyword ,@(ensure-list result)))))
         (format-accessors ()
           (let ((readers (closer-mop:slot-definition-readers slot))
                 (writers (closer-mop:slot-definition-writers slot)))
             (if (and readers writers)
                 (list :accessor (car readers))
                 `(,@(format-keyword :writer
                                     #'closer-mop:slot-definition-writers)
                     ,@(format-keyword :reader
                                       #'closer-mop:slot-definition-readers))))))
      `(,(closer-mop:slot-definition-name slot)
         ,@(format-accessors)
         ,@(format-keyword :allocation #'closer-mop:slot-definition-allocation
                           :bad-result :instance)
         ,@(format-keyword :initarg #'closer-mop:slot-definition-initargs)
         ,@(when (closer-mop:slot-definition-initform slot)
                `(:initform ,(closer-mop:slot-definition-initform slot)))
         ,@(format-keyword :type #'closer-mop:slot-definition-type
                           :bad-result t)
         ,@(when (documentation slot t)
                 `(:documentation ,(documentation slot t))))))

(macrolet ((define-mop (name function &optional generic-docstring)
               `(progn
                  (defgeneric ,name (class)
                    (:documentation ,generic-docstring))
                  (defmethod ,name ((class class))
                    (mapcar #',function (class-direct-slots class)))
                  (defmethod ,name ((instance standard-object))
                    (,name (class-of instance)))
                  (defmethod ,name ((class symbol))
                    (,name (find-class class)))
                  (defmethod ,name ((class string))
                    (,name (find-symbol class))))))
    (define-mop class-direct-slot-definitions generate-slot-specifier))

(defgeneric class-direct-slots->list (class))
(defmethod class-direct-slots->list ((class class))
  (mapcar #'generate-slot-specifier (closer-mop:class-direct-slots class)))

(defgeneric class-default-initargs->list (class))
(defmethod class-default-initargs->list ((class class))
  (mapcan #'butlast (closer-mop:class-default-initargs class)))

(defgeneric class-direct-default-initargs->list (class))
(defmethod class-direct-default-initargs->list ((class class))
  (let ((result (closer-mop:class-default-initargs class)))
    (when result
      `(:default-initargs ,@(mapcan #'butlast result)))))

(defgeneric class-direct-superclasses->list (class))
(defmethod class-direct-superclasses->list ((class class))
  (mapcar #'class-name (closer-mop:class-direct-superclasses class)))

(defgeneric store-class (class))
(defmethod store-class ((class class))
  `(defclass ,(class-name class)
       ,(class-direct-superclasses->list class)
     ,(class-direct-slots->list class)
     ,(class-direct-default-initargs->list class)
     ,(when (documentation class t)
             `(:documentation ,(documentation class t)))))
(defmethod store-class ((instance standard-object))
  (store-class (class-of instance)))