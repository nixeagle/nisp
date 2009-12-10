(defpackage #:nispbot
  (:use :common-lisp :lift
        :nisp
        :cl-irc :cl-ppcre
        :nispbot-config
        :nisp-introspect
        :nisp-safe)
  (:shadowing-import-from :cl-irc :pass))

(in-package :nispbot)

(deftestsuite root-suite (nisp::root-suite) ())
(deftestsuite basic-irc-suite (root-suite) ())


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

(deftestsuite test-make-irc-message (basic-irc-suite)
  ()
  (:test (format-private-message
           (:documentation "Demonstrate how to create a private message and
verify that the result is correct.")
           (ensure-same (irc::make-irc-message "privmsg" "#channel" "text")
                        (format nil "privmsg #channel :text~%")
                        :test #'string=))))

(deftestsuite test-tokenize-string (basic-irc-suite)
  ()
  (:documentation "Demonstrate irc::tokenize-string")
  (:test (split-by-space
          (ensure-same (irc::tokenize-string
                        "Some string to test on"
                        :delimiters '(#\Space))
                       '("Some" "string" "to" "test" "on")))))

(deftestsuite test-parse-bot-command (basic-irc-suite)
  ()
  (:test (short-command
          (ensure-same
           (parse-bot-command ",arglist +")
           (values "arglist" "+")
           :test #'string=))))

(deftestsuite test-function-lambda-list-to-string
    (basic-irc-suite)
  ()
  (:test (pass-+
          (:documentation "We need to get a string")
          (ensure (stringp (function-lambda-list-to-string "+")))))
  (:test (pass-1
          (:documentation "Invalid input like the number 1 in a string
needs to return a sensible result.")
          (ensure (stringp (function-lambda-list-to-string "1"))))))


(defpackage #:nispbot-basic-commands
  (:use :cl :lift :nispbot))

(in-package :nispbot-basic-commands)
