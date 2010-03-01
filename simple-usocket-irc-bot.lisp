(defpackage #:simple-usocket-irc-bot
  (:use :cl :usocket))
(in-package :simple-usocket-irc-bot)

(defvar *bot-socket* nil
  "Global socket for a single bot connection.")

(defun connect! (host &key (port 6667))
  (setq *bot-socket* (socket-connect host port)))

(defun format-irc-message (&rest args)
  "Format ARGS into a valid irc message sans the newlines at the end."
  (let ((result-string "")
        (last-arg (apply #'concatenate 'string ":" (last args)))
        (arg-list-but-last (append (butlast args))))
    (concatenate 'string
                 (dolist (arg arg-list-but-last result-string)
                   (setq result-string (concatenate 'string result-string arg " ")))
                 last-arg)))
(defun send-irc-message (&rest args)
  (prog1
      (write-sequence (apply #'format-irc-message args) (socket-stream *bot-socket*))
    (terpri (socket-stream *bot-socket*))
    (force-output (socket-stream *bot-socket*))))

;;; Minimal sequence to write a bot that joins and quits immediately
;;; closing the socket while doing so.
;;;
;;; To run this and other code snippets, remove the #+ () line in front
;;; and hit C-c C-c in emacs.
#+ ()
(progn
  (connect! "silicon.eighthbit.net")
  (send-irc-message "NICK" "lispbot")
  (send-irc-message "USER" "lisp" "0" "*" "hi from nixeagle!")
  (send-irc-message "JOIN" "#offtopic")
  (send-irc-message "PRIVMSG" "#offtopic" "Hi from nixeagle's simple lisp bot!")
  (send-irc-message "QUIT" "bye now!")
  (socket-close *bot-socket*))

;;; listen to the socket, don't hang if there is nothing to hear, but if
;;; there is, print it to #sonicircd
#+ ()
(when (listen (socket-stream *bot-socket*))
  (send-irc-message "PRIVMSG" "#sonicircd" (read-line (socket-stream *bot-socket*))))

;;; This will loop forever waiting for input and doing something (again
;;; just printing to #sonicircd) when the bot hears something
#+ ()
(loop
   (wait-for-input *bot-socket*
                   (send-irc-message "PRIVMSG" "#sonicircd"
                                     (read-line (socket-stream *bot-socket*)))))

;;; Ask me about threading when you get tired of stopping the loop just to
;;; modify the bot's behavior, but you need to know how to put the bot's
;;; behavior in a function and so on so that the modifications to the
;;; function can be seen by the threaded bot (this is not difficult, just
;;; put (defun i-got-input-from-server! (&optional (socket *bot-socket*))
;;; <some code here>) and call that instead of directly printing in the
;;; loop.

;;; Close a socket with
#+ ()
(socket-close *bot-socket*)