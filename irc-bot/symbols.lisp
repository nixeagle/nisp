(in-package :nisp.i)

(defpackage #:nisp.i.command-argument-symbols
  (:use))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *command-argument-symbols-package* :nisp.i.command-argument-symbols
    "Package to intern symbols for command argument parsing.")
  (nutils.packages:make-empty-package *command-argument-symbols-package* :supersede))

(defun split-command-string (command-string)
  "Split COMMAND-STRING into appropriate parts."
  (declare (type string command-string))
  (split-sequence #\Space command-string :remove-empty-subseqs t))

(defmethod ensure-string ((arg string))
  "Return ARG as it is already a string."
  arg)
(defmethod ensure-string ((arg symbol))
  (symbol-name arg))


(defun %generate-interned-argument-symbols (count &optional (format-spec "ARG~A"))
  "Intern as if by FORMAT-SPEC a list up to COUNT symbols."
  (declare (type non-negative-fixnum count))
  (iter (for n :from 1 :to count)
        (collect (format-symbol t format-spec n))))
(defun %format-command-handler-symbol (count &optional
                                       (format-spec "COMMAND-HANDLER-~A-ARGS"))
  (format-symbol t format-spec count))
(defparameter +command-routing-functions+
  (flet ((define-handler (count)
           "Define a `command-handler-COUNT-args' generic function."
           (ensure-generic-function
            (%format-command-handler-symbol count)
            :lambda-list `(connection sender to cmd
                                      ,@(%generate-interned-argument-symbols count)
                                      &optional remaining)
            :documentation (format nil "Handle commands with ~A arguments" count))))
    (let ((maximum-routing-arguments 15))
      (make-array (1+ maximum-routing-arguments)
                  :initial-contents
                  (iter (for n from 0 to maximum-routing-arguments)
                        (collect (define-handler n)))
                  :element-type 'STANDARD-GENERIC-FUNCTION)))
  "Array of generic functions for routing commands.")

(defun %find-command-handler-generic (count &optional
                                      (array +command-routing-functions+))
  "Find generic function with COUNT command parameters in VECTOR"
  (declare (type non-negative-fixnum count)
           (type simple-array array))
  (aref array count))


(defgeneric find-command-handler-generic (&rest arguments)
  (:documentation "Look up command handler by ARGUMENTS."))
(defmethod find-command-handler-generic (&rest more-arguments)
  (%find-command-handler-generic
   (typecase (car more-arguments)
     (string (length more-arguments))
     (integer (car more-arguments))
     (cons (length (car more-arguments)))
     (t 0))))

(defun %format-command-method-symbol (name)
  (declare (type symbol name))
  (let ((params (split-sequence #\- (symbol-name name))))
    (values (length params) (mapcar (lambda (x)
                                      (ensure-symbol
                                       x
                                       *command-argument-symbols-package*))
                                    params))))


(defmethod filter-to-remaining-arguments ((full-command string)
                                          (current-args list))
  "Convert CURRENT-ARGS to a string and call again."
  (filter-to-remaining-arguments full-command
                                 (format nil "~{~A~^ ~}" current-args)))
(defmethod filter-to-remaining-arguments ((full-command string)
                                          (current-args string))
  (subseq full-command (+ (search (string-upcase current-args)
                                  (string-upcase full-command))
                          (length current-args))))
(defmethod filter-to-remaining-arguments :around ((full-command string)
                                                  (current-args string))
  "Remove leading space if there is one."
  (let ((result (call-next-method)))
    (if (and (> (ARRAY-DIMENSION result 0) 0)
             (eq (aref result 0) #\Space))
        (subseq result 1)
        result)))