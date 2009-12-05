(defpackage :nix-bot (:use :common-lisp :irc :cl-ppcre))
(in-package :nix-bot)
(setq *allow-named-registers* t)

(defvar *connection*)
(defvar *nickname* "clnixbot")
(defconstant *eighthbit* "platinum.eighthbit.net")
(defvar *channel* "#lisp")

(defun start-connection ()
  "Simple connection function for basic testing."
  (set '*connection* (connect :nickname *nickname*
                             :server *eighthbit*))
  (join *connection* *channel*)
  (join *connection* "#bots")
  (irc:add-hook *connection* 'irc:irc-privmsg-message #'command-hook)
  (irc:read-message-loop *connection*))

(defun command-hook (message)
  "For now lets try to parse just one command"
  (let ((msg-text (second (arguments message)))
        (target (first (arguments message))))
    (handler-case
        (progn
          (multiple-value-bind (bot-cmd ar)
              (parse-bot-command msg-text)
            (when (string-equal bot-cmd "arglist")
              (privmsg (connection message)
                       target
                       (function-lambda-list-to-string ar)))))
      (error () (privmsg (connection message)
                         target
                         "An error has occurred")))))

(defun parse-bot-command (msg-text)
  "Parse an irc message and split command out from the rest."
  (register-groups-bind (command args)
        (",(\\w*) (.+)" msg-text)
    (values command args)))

(defun function-lambda-list-to-string (string)
  "Return a string with the function's arg list in it."
  (princ-to-string
   (sb-introspect:function-lambda-list
    (read-from-string string))))