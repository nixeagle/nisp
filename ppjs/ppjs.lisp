(defpackage #:ppjs
  (:use :cl :alexandria :eos))
(in-package :ppjs)

(nisp.i::define-simple-command ppjs
  (let* ((*read-eval* nil))
    (nisp.i::reply (format nil "js: ~A"
                           (remove #\newline
                                   (call-with-js-pprint-table
                                    (lambda ()
                                      (prin1-to-string (read-from-string (nisp.i::remaining-parameters))))))))))
(defvar *recursivep* nil)
(defvar *statementp* nil)
(defvar *functionp* nil)
(defvar *top-block-p* nil)
(defparameter *block-prefix* "    ")

(defun call-wrap-parens (*standard-output* thunk)
  (pprint-logical-block (nil nil)
    (when *recursivep* (princ #\())
    (let ((*recursivep* t))
      (funcall thunk))
    (when *recursivep* (princ #\)))))

(defmacro wrap-parens (stream &body body)
  `(call-wrap-parens ,stream (lambda () ,@body)))

(defun call-wrap-statement (*standard-output* thunk)
  (let ((*statementp* t))
    (pprint-indent :block 0)
    (funcall thunk))
  (unless *statementp*
    (write-char #\;)
    (unless *top-block-p*
      (pprint-newline :linear))))

(defmacro wrap-statement (stream &body body)
  `(call-wrap-statement ,stream (lambda () ,@body)))
(defmacro wrap-braces (stream &body body)
  `(call-wrap-braces ,stream (lambda () ,@body)))

(defun wrap-function (*standard-output* list)
  (let ((*recursivep* nil)
        (*statementp* nil))
    (pprint-logical-block (nil list)
      (pprint-pop)                      ;Ignore defun
      (princ "function ")
      (write (pprint-pop))
      (write-char #\Space)
      (pprint-function-lambda-list *standard-output* (pprint-pop))
      (write-char #\Space)
      (wrap-braces *standard-output*
        (let (next)
          (pprint-logical-block (*standard-output* (cdddr list))
            (pprint-exit-if-list-exhausted)
            (loop
               (setq next (pprint-pop))
               (pprint-exit-if-list-exhausted)
               (write next)))
          (princ "return ")
          (let ((*top-block-p* t))
            (write next)))))))

(defun call-wrap-braces (*standard-output* thunk)
  (write-char #\{)
  (pprint-newline :mandatory)
  (pprint-logical-block (*standard-output* nil)
    (princ *block-prefix*)
    (funcall thunk))
  (pprint-newline :linear)
  (write-char #\}))

(defun print-op (op)
  (pprint-indent :block 2)
  (write-char #\Space)
  (pprint-newline :fill)
  (princ op)
  (write-char #\Space))

(defmacro noppc (arg stream)
  `(let ((*print-pretty* nil))
     (princ ,arg ,stream)))

(defun call-print-infix-op (*standard-output* list printing-function)
  (pprint-logical-block (*standard-output* list)
    (let ((op (pprint-pop)))
      (loop
         (let ((next (pprint-pop)))
           (funcall printing-function next))
         (pprint-exit-if-list-exhausted)
         (print-op op)))))
(defmacro with-print-infix-op ((var list &optional (stream '*standard-output*)) &body body)
  `(call-print-infix-op ,stream ,list
                        (lambda (,var) ,@body)))
(defun pprint-+ (*standard-output* list)
  (wrap-statement *standard-output*
    (wrap-parens *standard-output*
      (with-print-infix-op (next list)
        (write (or next 0))))))

(defun pprint-* (*standard-output* list)
  (wrap-statement *standard-output*
    (wrap-parens *standard-output*
      (with-print-infix-op (next list)
        (write (or next 1))))))

(defun pprint-- (*standard-output* list)
  (wrap-statement *standard-output*
    (wrap-parens *standard-output*
      (with-print-infix-op (next list)
        (write next)))))

(defun pprint--/one-argument (*standard-output* list)
  (pprint-- *standard-output* (list (car list) (- (second list)))))

(defun pprint-/ (*standard-output* list)
  (wrap-statement *standard-output*
    (wrap-parens *standard-output*
      (with-print-infix-op (next list)
        (case next
          (0 (error 'division-by-zero :operation 'pprint-/
                    :operands (list 1 (cadr list))))
          (null (error "You cannot divide by 0 arguments."))
          (otherwise (write next)))))))

(defun pprint-//one-argument (*standard-output* list)
  (pprint-/ *standard-output* `(/ ,@(if  (= 1 (second list))
                                         (list 1)
                                         (list (second list))))))

(defun pprint-defun (stream list)
  (wrap-function stream list))

(defun pprint-function-lambda-list (stream list)
  (if list
      (prin1 list stream)
      (princ "()" stream)))

(defun pprint-symbol (stream symbol)
  (princ (string-downcase (symbol-name symbol)) stream))

(define-condition ppjs-program-error (program-error) ())
(define-condition function-error (ppjs-program-error)
  ((name :reader function-error-name
         :initarg :name
         :documentation "Function's name we are signaling about.")))

(define-condition argument-count-error (function-error)
  ((required :reader argument-count-error-required
             :initarg :required)
   (given :reader argument-count-error-given
          :initarg :given))
  (:report (lambda (condition stream)
             (format stream "The function ~S"
                     (function-error-name condition))
             (pprint-logical-block (stream nil)
               (pprint-indent :block 0 stream)
               (format stream " expects at least ~S argument~:p"
                       (argument-count-error-required condition))
               (pprint-newline :fill stream)
               (format stream " but got ~S argument~:p instead."
                       (argument-count-error-given condition))))))

(defun dispatch-argument-count-error (stream list &optional (required 1))
  (declare (ignore stream))
  (error 'argument-count-error
         :given (length (cdr list))
         :required required
         :name (car list)))

(defparameter *js-table*
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'symbol 'pprint-symbol)
    (set-pprint-dispatch '(cons (member defun)) 'pprint-defun 3)
    (set-pprint-dispatch '(cons (member +)) 'pprint-+ 5)
    (set-pprint-dispatch '(cons (member -)) 'pprint-- 5)
    (set-pprint-dispatch '(cons (member -) (cons number null)) 'pprint--/one-argument 6)
    (set-pprint-dispatch '(cons (member *)) 'pprint-* 5)
    (set-pprint-dispatch '(cons (member /)) 'pprint-/ 5)
    (set-pprint-dispatch '(cons (member /) (cons number null)) 'pprint--/one-argument)
    (set-pprint-dispatch '(cons (member - /) null)
                         'dispatch-argument-count-error 100)
    *print-pprint-dispatch*))
(defun js-pprint-table ()
  *js-table*)
(defun call-with-js-pprint-table (thunk)
  (declare (type function thunk))
  (let ((*print-pprint-dispatch* (js-pprint-table)))
    (funcall thunk)))

(defmacro ppjs (&body body)
  `(call-with-js-pprint-table (lambda () (prin1 ',@body))))

(defmacro ppjsi (&body body)
  `(irc:privmsg nisp.i::*devel-bot* "#bots"
               (format nil "js: ~A // ~A => ~S"
                       (remove #\newline (call-with-js-pprint-table (lambda () (prin1-to-string ',@body))))
                       (prin1-to-string',@body)
                       ,@body)))
