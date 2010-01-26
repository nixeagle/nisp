(in-package :cl-user)
(defpackage #:nisp.hunchentoot.alpha-site
  (:use :cl :hunchentoot))
(in-package :nisp.hunchentoot.alpha-site)

(defparameter *alpha-port* 1337
  "Default port for alpha-site.")

(defclass alpha-site-acceptor (acceptor)
  ()
  (:default-initargs :port *alpha-port*)
  (:documentation "Alpha site has its own acceptor so that testing hooks
can specialize on the acceptor class and not mess with the normal
hunchentoot acceptor."))

(defvar *alpha-acceptor* (make-instance 'alpha-site-acceptor)
  ;; Note there is an oddity that I cannot stop one of these and then
  ;; start it and expect it to work as I would normally intend. Calling
  ;; REINITIALIZE-INSTANCE on the instance did not do anything useful
  ;; either.
  ;;
  ;; So to restart the accepter, call (stop *alpha-acceptor*)
  ;; followed by: (setq *alpha-acceptor* (make-instance ...))
  ;; and then call (start *alpha-acceptor*).
  "Default acceptor.")

(defvar *alpha-last-request* nil
  "Contents of the last hunchentoot request.")

;;; Destructive toplevel settings to hunchentoot
(setq *message-log-pathname* "/home/james/n/www/message.log"
      *access-log-pathname* "/home/james/n/www/access.log"
      *lisp-errors-log-level* :info     ;Default is :error
      *lisp-warnings-log-level* :info   ;Default is :warning

      ;;On a real site this obviously should be set to nil,
      ;;hunchentoot's default for the normal security reasons.
      *show-lisp-errors-p* t)


(defun start-alpha-acceptor! ()
  "  Startup ACCEPTOR.

  When ACCEPTOR has already been shutdown, a brand new instance is
  made and that new instance is used instead."
  ;; ACCEPTOR used to be an optional argument. For the time being this
  ;; option has been removed to simplify starting and stopping just one
  ;; acceptor.
  (when (hunchentoot::acceptor-shutdown-p *alpha-acceptor*)
    (setq *alpha-acceptor* (make-instance 'alpha-site-acceptor)))
  (start *alpha-acceptor*))

;;; Modifications/around methods on hunchentoot generics.
(defmethod handle-request :before ((acceptor alpha-site-acceptor) request)
  "Set *ALPHA-LAST-REQUEST* to REQUEST."
  (setq *alpha-last-request* request)
  #+ () (setq *alpha-acceptor* *acceptor*))


;;; dispatching things ------------------------------
(defun default-handler ()
  "Called if nothing else matches."
  (log-message :info "Default dispatch called on alpha-site, script: ~A"
               (script-name*))
  "Nothing here!")

(setq *default-handler* 'default-handler)

;;; End hunchentoot-alpha.lisp (for magit/git)
