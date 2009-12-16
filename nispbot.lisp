(defpackage #:nispbot
  (:use :common-lisp :lift
        :nisp
        :metatilities
        :trivial-timeout
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
            :initform #\,)
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

(defgeneric is-eval-request (instance message))
(defmethod is-eval-request ((bot irc-bot) (msg irc-privmsg-message))
  (eq (char (second (arguments msg)) 0)
            (irc-bot-comchar bot)))

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
  (let ((forms (parse-eval-request (connection message) message))
        ( *print-readably* t))
    (when forms
      (handler-case
          (with-timeout (1)
            (privmsg (connection message)
                     (first (arguments message))
                     (strip-newlines
                      (format nil "~A"
                              (safe-eval message forms)))))
        (error (condition) (privmsg (connection message)
                                    (first (arguments message))
                                    (format nil "~A" condition)))))))

(defmethod safe-read ((msg irc-privmsg-message)
                       (forms string) &optional owner)
  (declare (ignore owner))
  (safe-read (irc-bot-safe (connection msg))
             forms
             (host msg)))


(defpackage #:nispbot-basic-commands
  (:use :cl :lift :nispbot))

(in-package :nispbot-basic-commands)