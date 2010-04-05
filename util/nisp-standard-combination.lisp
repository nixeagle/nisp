;;; Nisp standard method combination. This adds a few extra helpers above
;;; and beyond the normal method combination. This should be regarded as
;;; experimental and certainly not for use in anything critical.

(defpackage #:nisp-standard-method-combination
  (:use :cl :iterate :alexandria)
  (:export #:nisp-standard))

(in-package :nisp-standard-method-combination)

(defun method-specializers-unique-p (method list)
  (not (find (closer-mop:method-specializers method) list :test #'equal
         :key #'closer-mop:method-specializers)))

(defun check-unique-method-specializers (method list)
  (unless (method-specializers-unique-p method list)
    (error "Two methods are specialized on the same arguments.")))

(defun collecting-into-forms (list method keywords gensyms)
  "Generate (collecting METHOD :into GENSYM.

Given symbols for LIST and METHOD plus lists of KEYWORDS and GENSYMS,
generate a series of collecting into statements where the METHOD gets
shoved into one of the GENSYMs given in the list of GENSYMS."
  `(progn
     ,@(mapcar (lambda (keyword gen)
                 `(when (find ,keyword ,list)
                    (check-unique-method-specializers ,method ,gen)
                    (collecting ,method :into ,gen :result-type 'list)))
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
    (unless hook
      (mapl (lambda (sublist)
              (check-unique-method-specializers (car sublist) sublist))
            primary))
    (flet ((call-methods (methods)
             (mapcar (lambda (method)
                       `(call-method ,method))
                     methods))
           (wrap-method (around form)
             (if around
                 `(call-method ,(first around)
                               (,@(rest around)
                                  (make-method ,form)))
                 form)))
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
             (standard-form (wrap-method around form))
             (standard-around (wrap-method meta-around standard-form))
             (defaulting (reverse defaulting)))
        (wrap-method defaulting standard-around)))))