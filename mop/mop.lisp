(in-package :nisp.mop)

(defun compare-generic-applicable-methods (method-args original-method-args
                                           generic-function)
  "True if applicable methods for METHOD-ARGS `equal' set for ORIGINAL-METHOD-ARGS."
  (equal (compute-applicable-methods generic-function method-args)
         (compute-applicable-methods generic-function original-method-args)))

(define-condition applicable-methods-mismatch-error (error)
  ((old-args :initarg :old-args
             :reader applicable-methods-mismatch-error-original-args)
   (new-args :initarg :new-args
             :reader applicable-methods-mismatch-error-new-args)
   (generic-function :initarg :generic-function
                     :reader applicable-methods-mismatch-error-generic-function))
  (:report (lambda (condition stream)
             (let ((generic-function
                    (applicable-methods-mismatch-error-generic-function condition))
                   (original-args
                    (applicable-methods-mismatch-error-original-args condition))
                   (new-args
                    (applicable-methods-mismatch-error-new-args condition)))
               (format stream
                       "Applicable methods differ:~% ~
                        Generic function: ~S~% ~
                        Old args: ~S~% ~
                        New args: ~S~% ~
                        Old set: ~A~% ~
                        New set: ~A~%"
                       generic-function
                       original-args
                       new-args
                       (compute-applicable-methods generic-function original-args)
                       (compute-applicable-methods generic-function new-args))))))



