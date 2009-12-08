(defpackage #:nispbot
  (:use :common-lisp  
        :nisp
        #+5am :5am
        :cl-irc :cl-ppcre
       
        :nispbot-config
        :nisp-introspect)
  (:shadowing-import-from :cl-irc :pass))

(in-package :nispbot)

(5am:def-suite basic-irc-suite
    :in nisp::all-tests
    :description "Tests all the irc related stuff")


(setq *allow-named-registers* t)

(defvar *connection*)

(defun start-connection ()
  "Simple connection function for basic testing."
  (set '*connection* (connect :nickname *nickname*
                             :server *eighthbit*))
  (when (stringp *channel*)
      (warn "*channel* will be removed in a future release. Please use *channels*")
      (join *connection* *channel*))
  (join-all-channels)
  (irc:add-hook *connection* 'irc:irc-privmsg-message #'command-hook)
  (irc:start-background-message-handler *connection*))

(defun join-all-channels ()
  "Join all channels in *channels*. Later we will expand this to work
for any arbitrary connection or list of channels."
  (mapc
   (lambda (channel)
     (join *connection* channel))
   nispbot-config::*channels*))

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

(defpackage #:nispbot-basic-commands
  (:use :cl #+5am :5am :nispbot))

(in-package :nispbot-basic-commands)

(5am:def-suite basic-command-suite
    :in nispbot::basic-irc-suite)

(5am:in-suite basic-command-suite)
