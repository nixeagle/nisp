(in-package :cl-user)
(defpackage #:nisp.www
  (:use :cl :hunchentoot :hunchentoot-vhost :alexandria :cl-who :parenscript)
  (:shadowing-import-from :parenscript :switch :var)
  (:nicknames :www))
(in-package :nisp.www)

(defun debug-to-irc (arg)
  (print arg)
  #+nisp-devel
  (list (irc:privmsg nisp.i::*devel-bot* "#programming" arg)
                    #+ ()   (irc:privmsg nisp.i::*freenode* "#botters" arg))
  #+nisp-vps
  (irc:privmsg nisp.i::*9b* "#programming" arg))


(setq *prologue* "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
  (setf (html-mode) :SGML)
(setq *js-string-delimiter* #\")

(defmacro with-html (&body body)
  "Do with-html-output-to-string with BODY.

This is the sensible and sane default that hunchentoot does not seem to
provide on its own."
  `(progn

     (with-html-output-to-string (*standard-output* nil #+ () :prologue #+ () t)
       ((:html ;:lang "en-US"
               ;:xmlns "http://www.w3.org/1999/xhtml"
               )
        ,@body))))

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
(defvar *nisp-8080-acceptor* (make-instance 'nisp-1337-acceptor :port 8080 :name :chirps))
(defvar *nisp-6667-acceptor* (make-instance 'nisp-1337-acceptor :port 6667 :name :chirps))

(defvar *nisp-started-6667-acceptor* (start *nisp-6667-acceptor*))
(defvar *nisp-started-8080-acceptor* (start *nisp-8080-acceptor*))
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


(defparameter *commits*
  (hunchentoot-vhost:make-virtual-host "commits"
                                       '(#+nisp-vps "commits.nixeagle.net"
                                         #+nisp-devel "pri.wiki.james.nixeagle.net")
                                       :server
                                       #+nisp-vps
                                       (assoc-value cl-user::*root-ports* 80)
                                       #+nisp-devel
                                       *nisp-started-8080-acceptor* #+ ()
                                       (list
                                             *nisp-started-1337-acceptor*
                                             *nisp-started-6667-acceptor*)))

(defvar *nisp-started-1337-acceptor*
  (when (eq (nisp.global::system-keyword) :nisp-devel)
    (start *nisp-1337-acceptor*)))



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

(defvar *github-request*)
;;; Just messing with github hooks
(defun decode-github-hook (request)
  (setq *github-request* request)
  (json:decode-json-from-string
   (cdr (assoc "payload" (post-parameters request) :test #'equalp))))

(defun github-compare-view (repository before after)
  (format nil "~A/compare/~A...~A"
          repository before after))

(defun github-compare-view-from-payload (payload-alist)
  (declare (type list payload-alist))
  (github-compare-view
   (assoc-value (assoc-value payload-alist :repository) :url)
   (assoc-value payload-alist :before)
   (assoc-value payload-alist :after)))

(defun github-address-p (octets &key
                         (octets-begin (list 207 97 227 224))
                         (octets-end (list 207 97 227 255)))
  (every #'<= octets-begin octets octets-end))

(defun format-github-commit-message (alist-message)
  (let ((commits (assoc-value alist-message :commits))
        (repository (assoc-value alist-message :repository)))
    (apply #'list
           (format nil "~A/~A: ~D new commits, compare view at <~A>. ~:[~;~D outstanding issues.~]"
                   (assoc-value (assoc-value repository :owner) :name)
                   (assoc-value repository :name)
                   (length commits)
                   (nisp.i::shorturl-is.gd
                    (github-compare-view-from-payload
                     alist-message))
                   (assoc-value repository :has-issues)
                   (assoc-value repository :open-issues))
           (loop for commit in commits
              collect
                (format nil "~A:~
                             ~@[ Added ~{~A~^, ~}.~]~
                             ~@[ Modified ~{~A~^, ~}.~]~
                             ~@[ Removed ~{~A~^, ~}.~] ~
                             With summary: ~A"
                      (assoc-value (assoc-value commit :author) :name)
                      (assoc-value commit :added)
                      (assoc-value commit :modified)
                      (assoc-value commit :removed)
                      (subseq (assoc-value commit :message) 0 (position #\NewLine (assoc-value commit :message))))))))

(define-easy-virtual-handler *commits*
    (commits-github :uri "/github") (payload)
  (setf *github-request* *request*)
  (if (github-address-p (mapcar #'parse-integer
                                (split-sequence:split-sequence #\. (remote-addr*))))
      (let ((sexp  (json:decode-json-from-string payload)))
        (loop for commit in (format-github-commit-message sexp)
           do (debug-to-irc commit))
#+ ()        (debug-to-irc (format nil "Compare view push: ~A"
                              ;(assoc-value (cdar sexp) :name)
                              (nisp.i::shorturl-is.gd
                               (github-compare-view-from-payload
                                sexp)))))
      "You are not github, bye now!"))

(define-easy-handler (github-hook :uri "/github-hook")
    (payload)
  (setf *github-request* *request*)
  (if (github-address-p (mapcar #'parse-integer
                                (split-sequence:split-sequence #\. (remote-addr*))))
      (let ((sexp  (json:decode-json-from-string payload)))
        (loop for commit in (format-github-commit-message sexp)
           do (debug-to-irc commit)))
      "You are not github, bye now!"))

;;; End hunchentoot-alpha.lisp (for magit/git)
