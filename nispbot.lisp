
(in-package :nispbot)
(setq *allow-named-registers* t)

(defvar *connection*)

(defun start-connection ()
  "Simple connection function for basic testing."
  (set '*connection* (connect :nickname *nickname*
                             :server *eighthbit*))
  (join *connection* *channel*)
  (join *connection* "#bots")
  (irc:add-hook *connection* 'irc:irc-privmsg-message #'command-hook)
  (irc:start-background-message-handler *connection*))

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

(defgeneric function-lambda-list-to-string (symbol)
  (:method (symbol)
    "An unexpected error has occured on your request, please notify the developers.")
  (:documentation
   "Return a string with the function's arg list in it."))


(defmethod function-lambda-list-to-string ((symbol string))
  (princ-to-string (function-lambda-list symbol)))


