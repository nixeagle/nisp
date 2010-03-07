;;;; Package for including all sorts of interesting and useful libraries
;;;; for testing.
(in-package :cl-user)

(defpackage #:nisp.user
  (:use :cl
        ;; implentation independent sockets
        :with-fbound
        :eos
        :usocket
        :drakma)                         ;http-request
  (:nicknames :nu :~))

(in-package :nisp.user)
;;; In custom userland, this userland has modifications from a standard
;;; userland.

(defmacro n.disasemble (form)
  (let ((s (gensym)))
    `(let ((,s (with-output-to-string (*standard-output*)
                 (disassemble ',(car form)))))
       (print ,s)
       (values
        ,form
        (count #\Newline ,s)))))
(defun ensure-no-leading-single-quote (string)
  "Append a leading ' to STRING if there is not already one."
  (declare (type string string))
  (if (char= (char string 0) #\')
      (subseq string 1)
      string))

(define-condition nisp-user-error (error)
  ())

(define-condition package-not-found-error (nisp-user-error)
  ((package :initarg :package :reader package-not-found-error-package))
  (:report (lambda (condition stream)
             (format stream "Package ~A is not loaded"
                     (package-not-found-error-package condition)))))

(define-condition swank-package-not-found-error (package-not-found-error)
  ()
  (:default-initargs :package :swank))

(defgeneric eval-in-emacs (form &optional nowait))
(defmethod eval-in-emacs ((form string) &optional nowait)
  (eval-in-emacs (read-from-string (ensure-no-leading-single-quote form))
                 nowait))
(defmethod eval-in-emacs (form &optional nowait)
  (let ((swank::*emacs-connection* (swank::default-connection)))
    (swank::eval-in-emacs form nowait)))
;;; END