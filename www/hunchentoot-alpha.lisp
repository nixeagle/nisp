(in-package :cl-user)
(defpackage #:nisp.www
  (:use :cl :hunchentoot))
(in-package :nisp.www)

(defparameter *nisp-port* 80
  "Default port for alpha-site.")

(defclass nisp-site-acceptor (acceptor)
  ()
  (:default-initargs :port *nisp-port*)
  (:documentation "Alpha site has its own acceptor so that testing hooks
can specialize on the acceptor class and not mess with the normal
hunchentoot acceptor."))

(defvar *nisp-acceptor* (make-instance 'nisp-site-acceptor)
  ;; Note there is an oddity that I cannot stop one of these and then
  ;; start it and expect it to work as I would normally intend. Calling
  ;; REINITIALIZE-INSTANCE on the instance did not do anything useful
  ;; either.
  ;;
  ;; So to restart the accepter, call (stop *alpha-acceptor*)
  ;; followed by: (setq *alpha-acceptor* (make-instance ...))
  ;; and then call (start *alpha-acceptor*).
  "Default acceptor.")

(defvar *nisp-last-request* nil
  "Contents of the last hunchentoot request.")

;;; Modifications/around methods on hunchentoot generics.
#+ ()
(defmethod handle-request :before ((acceptor nisp-site-acceptor) request)
  "Set *ALPHA-LAST-REQUEST* to REQUEST."
  (setq *nisp-last-request* request)
  #+ () (setq *alpha-acceptor* *acceptor*))


;;; dispatching things ------------------------------
(defun default-handler ()
  "Called if nothing else matches."
  (log-message :info "Default dispatch called on nisp-site, script: ~A"
               (script-name*))
  "Nothing here!")

(setq *default-handler* 'default-handler)

;;; End hunchentoot-alpha.lisp (for magit/git)
