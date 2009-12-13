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

(defclass irc-bot ()
  ((nick :accessor bot-nick
         :initform *nickname*
         :initarg :nick)
   (server :accessor bot-server
           :initform *server*
           :initarg :server)
   (connection :accessor bot-connection
               :initarg :connection
               :initform nil)))

(defun make-irc-bot (nick server)
  (make-instance 'irc-bot
                 :nick nick
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

;; (defun start-connection ()
;;   "Simple connection function for basic testing."
;;   (set '*connection* (connect :nickname *nickname*
;;                              :server *eighthbit*))
;;   (join-all-channels)
;;   (irc:add-hook *connection* 'irc:irc-privmsg-message #'command-hook)
;;   (irc:start-background-message-handler *connection*)
;; ;  (irc:read-message-loop *connection*)
;;   )

(defun join-all-channels (&optional (connection *connection*))
  "Join all channels in *channels*. Later we will expand this to work
for any arbitrary connection or list of channels."
  (mapc
   (lambda (channel)
     (join connection channel))
   nispbot-config::*channels*))

(defun command-hook (message)
  (declare (notinline command-hook))
  "For now lets try to parse just one command"
  (let ((msg-text (second (arguments message)))
        (target (first (arguments message))))
    (handler-case
        (progn
          (multiple-value-bind (bot-cmd)
              (parse-bot-command msg-text)
            (when bot-cmd
              (with-timeout (1)
                (privmsg (connection message)
                         target
                         (strip-newline
                          (format nil "~S"
                                  (multiple-value-bind (res)
                                      (eval (read-bot-message bot-cmd))
                                    res))))))
            ))
      (error (condition) (privmsg (connection message)
                                  target
                                  (format nil "~A" condition))))))

;; (when (string-equal bot-cmd "arglist")
;;               (privmsg (connection message)
;;                        target
;;                        (function-lambda-list-to-string ar)))


(defun parse-bot-command (msg-text)
  "Parse an irc message and split command out from the rest."
  (register-groups-bind (command)
        ("^,(.+)" msg-text)
    (values command)))

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
