;;; Nisp standard method combination. This adds a few extra helpers above
;;; and beyond the normal method combination. This should be regarded as
;;; experimental and certainly not for use in anything critical.

(defpackage #:nisp-standard-method-combination
  (:use :cl :iterate :alexandria))

(in-package :nisp-standard-method-combination)

(defun collecting-into-forms (list method keywords gensyms)
  "Generate (collecting METHOD :into GENSYM.

Given symbols for LIST and METHOD plus lists of KEYWORDS and GENSYMS,
generate a series of collecting into statements where the METHOD gets
shoved into one of the GENSYMs given in the list of GENSYMS."
  `(progn
     ,@(mapcar (lambda (keyword gen)
                 `(when (find ,keyword ,list)
                    (if (find (closer-mop:method-specializers ,method) ,gen :test #'equal
                              :key #'closer-mop:method-specializers)
                        (error "Two methods are specialized on the same arguments.")
                        (collecting ,method :into ,gen :result-type 'list))))
               keywords
               gensyms)))

(defmacro collect-normal-combinations
    (methods &rest combo-keywords)
  "Group METHODS by COMBO-KEYWORDS plus primary methods.

The return from this function is:
 (values primary-methods
        (first COMBO-KEYWORDS)
        (second COMBO-KEYWORDS)
        ...
        (last COMBO-KEYWORDS))

The values are all groups of methods that correspond to the qualifiers
passed in as COMBO-KEYWORDS.

For example if a method is an :AROUND method, and COMBO-KEYWORDS
contains :AROUND, that method (along with all others qualified as :AROUND
will be grouped together."
  (let ((gensyms (iter (for it :in combo-keywords)
                        (collect (gensym)))))
    (with-gensyms (method qualifiers intersection primary)
      `(iter (for ,method in ,methods)
             (for ,qualifiers = (method-qualifiers ,method))
             (for ,intersection = (intersection ,qualifiers ',combo-keywords))
             (cond
               ((length= 1 ,intersection)
                ,(collecting-into-forms `,qualifiers `,method combo-keywords gensyms))
               ((length= 0 ,intersection)
                (collecting ,method :into ,primary))
               (t (error "Too many combination keywords for normal-combinations.")))
             (finally (return
                        (values ,primary ,@gensyms)))))))

(define-method-combination nisp-standard (&key hook)
  ((methods *))
  (multiple-value-bind (primary defaulting meta-around
                                around before after)
      (collect-normal-combinations methods
        :defaulting :meta-around :around :before :after)
    (flet ((call-methods (methods)
             (mapcar (lambda (method)
                       `(call-method ,method))
                     methods)))
      (let* ((form (if (or before after (rest primary))
                       `(multiple-value-prog1
                            (progn ,@(call-methods before)
                                   ,(if hook
                                        `(,hook ,@(mapcar (lambda (method)
                                                            `(call-method ,method))
                                                          primary))
                                        `(call-method ,(first primary)
                                                      ,(rest primary))))
                          ,@(call-methods (reverse after)))
                       `(call-method ,(first primary))))
             (standard-form
              (if around
                  `(call-method ,(first around)
                                (,@(rest around)
                                   (make-method ,form)))
                  form))
             (standard-around
              (if meta-around
                  `(call-method ,(first meta-around)
                                (,@(rest meta-around)
                                   (make-method ,standard-form)))
                  standard-form))
             (defaulting (reverse defaulting)))
        (if defaulting
            `(call-method ,(first defaulting)
                          (,@(rest defaulting)
                             (make-method ,standard-around)))
            standard-around)))))