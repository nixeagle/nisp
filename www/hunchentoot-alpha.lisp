(in-package :cl-user)
(defpackage #:nisp.hunchentoot.alpha-site
  (:use :cl :hunchentoot))
(in-package :nisp.hunchentoot.alpha-site)

(defparameter *alpha-port* 1337
  "Default port for alpha-site.")

(defvar *alpha-acceptor* (make-instance 'acceptor :port *alpha-port*)
  ;; Note there is an oddity that I cannot stop one of these and then
  ;; start it and expect it to work as I would normally intend. Calling
  ;; REINITIALIZE-INSTANCE on the instance did not do anything useful
  ;; either.
  ;;
  ;; So to restart the accepter, call (stop *alpha-acceptor*)
  ;; followed by: (setq *alpha-acceptor* (make-instance ...))
  ;; and then call (start *alpha-acceptor*).
  "Default acceptor.")

(defun start-alpha-acceptor! ()
  "  Startup ACCEPTOR.

  When ACCEPTOR has already been shutdown, a brand new instance is
  made and that new instance is used instead."
  ;; ACCEPTOR used to be an optional argument. For the time being this
  ;; option has been removed to simplify starting and stopping just one
  ;; acceptor.
  (when (hunchentoot::acceptor-shutdown-p *alpha-acceptor*)
    (setq *alpha-acceptor* (make-instance 'acceptor :port *alpha-port*)))
  (start *alpha-acceptor*))

;;; End hunchentoot-alpha.lisp (for magit/git)
