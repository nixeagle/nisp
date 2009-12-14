(defpackage #:nispbot
  (:use :common-lisp :lift
        :nisp
        :metatilities
        :trivial-timeout
        :cl-irc :cl-ppcre
        :nispbot-config
        :nisp-empty-package
        :nisp-safe)
  (:shadowing-import-from :cl-irc :pass))

(in-package :nispbot)

(deftestsuite root-suite (nisp::root-suite) ())
(deftestsuite basic-irc-suite (root-suite) ())


(setq *allow-named-registers* t)

(defvar *connection*)

(defvar *nispbot*)

(defclass irc-bot (irc:connection)
  ((comchar :accessor irc-bot-comchar
            :initarg :comchar
            :initform #\,)))

(defun make-irc-bot (nick server)
  (connect :nickname nick :connection-type 'irc-bot
           :server server))

(defgeneric start-connection (instance))

(defmethod start-connection ((bot irc-bot))
  (setf (bot-connection bot)
        (connect :nickname (bot-nick bot)
                 :server (bot-server bot)))
  (join-all-channels (bot-connection bot))
  (irc:add-hook (bot-connection bot)
                'irc:irc-privmsg-message
                #'command-hook)
  (irc:read-message-loop (bot-connection bot)))

(defgeneric join-all-channels (instance)
  (:documentation "Join all channels in *channels*. Later we will expand this to work for any arbitrary connection or list of channels."))

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



(defun command-hook (message)
  (declare (notinline command-hook))
                                        ;  (print message)
  "For now lets try to parse just one command"
  
  (let (( forms (parse-eval-request (connection message) message)))
    (when forms
      (handler-case
          (with-timeout (1)
            (privmsg (connection message)
                     (first (arguments message))
                     (strip-newline
                      (format nil "~S"
                              (multiple-value-bind (res)
                                  (eval (read-bot-message forms))
                                res)))))
        (error (condition) (privmsg (connection message)
                                    target
                                    (format nil "~A" condition)))))))

(defun read-bot-message (msg-text)
  "Return a form ready to be funcall'd"
  (multiple-value-bind (res)
      (with-package (gen-empty-package)
        (cl::use-package :nisp-unsafe-iteration)
        (cl::use-package nisp-safe::*prepared-safe-packages*)
        (with-safe-readtable
          (read-from-string msg-text)))
    res))

(defun strip-newline (string)
  "Given a string, remove all newlines.

This is very irc specific where lines need to be all on one line.

Note that the newline is not replaced by a space!"
  (coerce
   (loop for char in (coerce string 'list)
      if (not (eq #\Newline  char))
      collect char)
   'string))

(deftestsuite strip-newline (root-suite)
  ()
  (:test (pass-string
          (:documentation "Base case")
          (ensure (stringp (strip-newline "some string"))))))

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
           (values "arglist +")
           :test #'string=))))


(defpackage #:nispbot-basic-commands
  (:use :cl :lift :nispbot))

(in-package :nispbot-basic-commands)


(defpackage #:nispbot-dev-helpers
  (:use :cl :nispbot :lift))

(in-package :nispbot-dev-helpers)

(defun reload-irc-privmsg-hook (&rest hooks)
  "Easy way to reset the hooks for privmsgs

Something like:

 (nispbot-dev-helpers::reload-irc-privmsg-hook #'nispbot::command-hook)"
  (irc:remove-hooks nispbot::*connection* 'irc:irc-privmsg-message)
  (mapc (lambda (hook)
          (irc:add-hook nispbot::*connection* 'irc:irc-privmsg-message hook))
        hooks))
