(in-package :cl-user)
(defpackage #:nisp.www
  (:use :cl :hunchentoot)
  (:nicknames :www))
(in-package :nisp.www)

(defun debug-to-irc (arg)
  (case (nisp.global::system-keyword)
    (:nisp-devel (irc:privmsg nisp.i::*devel-bot* "#nixeagle" arg))
    (:nisp-production (irc:privmsg nisp.i::*bot* "#nixeagle" arg))))

(defparameter *nisp-port* 80
  "Default port for alpha-site.")

(defclass nisp-site-acceptor (acceptor)
  ()
  (:default-initargs :port *nisp-port*)
  (:documentation "Alpha site has its own acceptor so that testing hooks
can specialize on the acceptor class and not mess with the normal
hunchentoot acceptor."))

(defclass nisp-1337-acceptor (nisp-site-acceptor)
  ()
  (:default-initargs :port 1337 :name :leet)
  (:documentation "Messing around on port 1337 mostly. This is just a
  silly and near worthless pun with numbers meaning 'leet'."))

(defvar *nisp-1337-acceptor* (make-instance 'nisp-1337-acceptor))

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

(defvar *old-default-handler* *default-handler*
  "Value of default-handler before we mess with it.")

(defun set-default-handler! (&optional reset)
  (if reset
      (setq *default-handler* *old-default-handler*)
      (setq *default-handler* 'default-handler)))


;;; Just messing with github hooks
(defun decode-github-hook (request)
  (json:decode-json-from-string
   (cdr (assoc "payload" (post-parameters request) :test #'equalp))))

(defun github-compare-view (repository before after)
  (format nil "~A/compare/~A...~A"
          repository before after))

;;; End hunchentoot-alpha.lisp (for magit/git)
