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
(defvar *function-level* 0)

(defun call-wrap-parens (stream thunk)
  (when *recursivep* (princ #\( stream))
  (let ((*recursivep* t))
    (funcall thunk))
  (when *recursivep* (princ #\) stream)))

(defmacro wrap-parens (stream &body body)
  `(call-wrap-parens ,stream (lambda () ,@body)))

(defun call-wrap-statement (*standard-output* thunk)
  (let ((*statementp* t))
    (pprint-indent :block 0)
    (funcall thunk *standard-output*)
    (unless *recursivep*
      (write-char #\;)
      (pprint-newline :linear))))
(defmacro wrap-statement (stream &body body)
  `(call-wrap-statement ,stream (lambda (*standard-output*) ,@body)))

(defun wrap-function (*standard-output* list)
  (pprint-logical-block (nil list)
    (pprint-pop)                        ;Ignore defun
    (pprint-indent :current 0)
    (princ "function ")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-function-lambda-list *standard-output* (pprint-pop))
    (write-char #\Space)
    (wrap-braces *standard-output* (cdddr list))))

(defparameter *block-prefix* "    ")
(defun wrap-braces (*standard-output* thunk)
  (write-char #\{)
  (pprint-newline :mandatory)
  (princ *block-prefix*)
  (funcall thunk)
  (write-char #\}))

(defun print-op (op)
  (pprint-indent :block 2)
  (write-char #\Space)
  (pprint-newline :fill)
  (princ op)
  (write-char #\Space))
*print-miser-width*
(defmacro noppc (arg stream)
  `(let ((*print-pretty* nil))
     (princ ,arg ,stream)))
(defun pprint-+ (*standard-output* list)
  (wrap-statement *standard-output*
    (pprint-logical-block (*standard-output* list)
      (let ((op (pprint-pop)))
        (loop
           (write (pprint-pop))
           (pprint-exit-if-list-exhausted)
           (print-op op)))
      (pprint-exit-if-list-exhausted))))

(defun pprint-* (stream list)
  (wrap-statement stream (cdr list)
                  #+ () (if (not (null (cdr list)))
                      (wrap-parens stream
                        (funcall (formatter "~{~S~^ * ~}") stream))
                      (princ 1 stream))))

(defun pprint-- (stream list)
  (wrap-statement stream (cdr list)
                #+ ()  (case (length)
                    (1 (princ "-" stream)
                       (prin1 (cadr list) stream))
                    (0 (error "You cannot subtract with 0 arguments!"))
                    (otherwise
                     (wrap-parens stream
                       (funcall (formatter "~{~S~^ - ~}") stream (cdr list)))))))

(defun pprint-/ (stream list)
  (wrap-statement stream (cdr list)#+ ()
                  (case (length (cdr list))
                    (1 (when (and (numberp (cadr list)) (zerop (cadr list)))
                         (error 'division-by-zero :operation 'pprint-/
                                :operands (list 1 (cadr list))))
                       (princ "1/" stream)
                       (prin1 (cadr list) stream))
                    (0 (error "You cannot divide by 0 arguments."))
                    (otherwise
                     (wrap-parens stream
                       (funcall (formatter "~{~S~^ / ~}") stream (cdr list)))))))

(defun pprint-defun (stream list)
  (wrap-function stream list))

(defun pprint-function-lambda-list (stream list)
  (if list
      (prin1 list stream)
      (princ "()" stream)))

(defun pprint-symbol (stream symbol)
  (princ (string-downcase (symbol-name symbol)) stream))

(defparameter *js-table*
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
    (set-pprint-dispatch 'symbol 'pprint-symbol)
    (set-pprint-dispatch '(cons (member +)) 'pprint-+)
    (set-pprint-dispatch '(cons (member -)) 'pprint--)
    (set-pprint-dispatch '(cons (member *)) 'pprint-*)
    (set-pprint-dispatch '(cons (member /)) 'pprint-/)
    (set-pprint-dispatch '(cons (member defun)) 'pprint-defun)
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
