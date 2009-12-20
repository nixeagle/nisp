(defpackage #:nispbot
  (:use :common-lisp :lift
        :nisp
        :metatilities
        :cl-irc :cl-ppcre
        :nispbot-config
        :nisp-empty-package
        :nisp-safe
        :nistilities)
  (:shadowing-import-from :cl-irc :pass))

(in-package :nispbot)

;;; cl-ppcre registers. Do we need this? --nixeagle
(setq *allow-named-registers* t)

(defvar *connection*)

(defvar *nispbot*)

(defclass irc-bot (irc:connection)
  ((comchar :accessor irc-bot-comchar
            :initarg :comchar
            :initform nispbot-config::*comchar*)
   (developer-host :accessor irc-bot-developer-host
                   :initarg :developer-host
                   :initform nispbot-config::*developer-host*)
   (safe :accessor irc-bot-safe
         :initform (make-safe-set))))

(defun make-irc-bot (nick server)
  (connect :nickname nick :connection-type 'irc-bot
           :server server
           :password nispbot-config::*password*))

(defgeneric join-all-channels (instance)
  (:documentation "Join all channels in *channels*. Later this will be expanded to use the connection's channel list."))

(defmethod join-all-channels ((bot irc-bot))
  (mapc
   (lambda (channel)
     (join bot channel))
   nispbot-config::*channels*))

(defgeneric reset-command-hook (instance))
(defmethod reset-command-hook ((bot irc-bot))
  (irc:remove-hooks bot 'irc:irc-privmsg-message)
  (irc:add-hook bot 'irc:irc-privmsg-message #'command-hook))

(defgeneric parse-eval-request (instance message))
(defmethod parse-eval-request ((bot irc-bot) (msg irc-privmsg-message))
  "might want to return our own message type eventually"
  (when (is-eval-request bot msg)
    (substring (second (arguments msg)) 1)))
(defmethod parse-eval-request ((bot irc-bot) (msg string))
  (when (is-eval-request bot msg)
    (substring msg 1)))
(defmethod parse-eval-request (bot msg)
  "If we get nil as a message, return nil"
  (declare (ignore bot))
  (when msg
    (error "Failed to parse message ~A" msg))
  nil)

(defgeneric is-eval-request (instance message))
(defmethod is-eval-request ((bot irc-bot) (msg irc-privmsg-message))
  (is-eval-request bot (second (arguments msg))))
;;; return something more useful then this...
(defmethod is-eval-request ((bot irc-bot) (msg string))
  (and (< 0 (length msg))
       (eq (char msg 0) (irc-bot-comchar bot))))

(defgeneric safe-eval (instance forms))
(defmethod safe-eval ((message irc:irc-privmsg-message) forms)
  (let ((read-result (safe-read message forms)))
    (multiple-value-bind (res)
                 (let ((*package* (nisp-safe::safe-package
                           (safe-select (irc-bot-safe (connection message))
                                                     (host message)))))
                   (cl::eval read-result))
      res)))

(defun command-hook (message)
  (declare (notinline command-hook))
                                        ;  (print message)
  "For now lets try to parse just one command"
  (let* ((forms (parse-eval-request (connection message) message))
         (admin-request (parse-eval-request (connection message)
                                            forms)))
    (when forms
      (handler-case
          (trivial-timeout:with-timeout (1)
            (if (and (string= nispbot-config::*developer-host*
                               (host message))
                     admin-request)
                ;; User is person running the bot, so allow any lisp to
                ;; be evaluated by that person.
                (privmsg (connection message)
                     (first (arguments message))
                     (strip-newlines
                      (format nil "~A"
                              (with-package :nispbot
                                (eval (read-from-string admin-request))))))
                ;; Untrusted users, eval their stuff in sandboxes
                (privmsg (connection message)
                         (first (arguments message))
                         (strip-newlines
                          (format nil "~A"
                                  (safe-eval message forms))))))
        (end-of-file (condition) (values nil condition))
        (error (condition) (privmsg (connection message)
                                    (first (arguments message))
                                    (format nil "~A" condition)))))
))

#+nil
(defun parse-links (string)
  (let ((it ()))
  (cl-ppcre:do-matches-as-strings (var "\\\[\\\[(.*?)\\\]\\\]" string it)
    (describe var)
    (push var it))))

(defun matches-list (reg &rest strings)
  (let ((ret ()))
    (dolist (str strings ret)
      (push
       (second (multiple-value-list
                (cl-ppcre:scan-to-strings reg str))) ret))))


(defmethod safe-read ((msg irc-privmsg-message)
                       (forms string) &optional owner)
  "Read given IRC message in the package corresponding to requesters hostmask"
  (declare (ignore owner))
  (safe-read (irc-bot-safe (connection msg))
             forms
             (host msg)))

(defun pull ()
  "Pull the source from github."
  (trivial-shell:shell-command "git pull"))