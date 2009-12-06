
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

(in-suite basic-irc-suite)

(test (make-irc-message)
   "Testing usage of `cl-irc:make-irc-message'"
   (is (string=
        (format nil "privmsg #channel :text~%")
        (irc::make-irc-message "privmsg" "#channel" "text"))
       "Simplest case of a private message to a channel"))

(test (tokenize-string)
  "Demonstrate irc::tokenize-string"
  (is (equal (irc::tokenize-string
          "Some string to test on"
          :delimiters '(#\Space))
         '("Some" "string" "to" "test" "on"))))

(test parse-bot-command
  (is-every string=
    ((nispbot::parse-bot-command ",arglist +")
     "arglist")
    ((multiple-value-bind (ignore arg) (nispbot::parse-bot-command ",arglist xyz")
       arg)
     "xyz")))

(test (function-lambda-list-to-string :depends-on parse-bot-command)
  "Get a valid arglist with no errors."
  (is
   (string=
    
    "(&REST ARGS)"
    (nispbot::function-lambda-list-to-string "+")))
  (5am:finishes
    (nispbot::function-lambda-list-to-string "1")
    "Do better then throwing an error on non-function objects"))