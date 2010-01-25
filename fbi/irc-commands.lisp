(defpackage #:nisp.fbi.irc-commands
  (:use :cl :usocket :iterate :json :nisp.util-protocol
        :nisp.util-types
        :bordeaux-threads
        :nisp.fbi.json-classes
        :nisp.fbi.sockets))

(in-package :nisp.fbi.irc-commands)
(defun make-keyword (string)
  "Convert STRING to a keyword (uppercased)."
  (declare (type string string))
  (intern (string-upcase string) :keyword))

(defgeneric irc-command-hook (command-string json socket)
  (:documentation "Act on COMMAND-STRING with JSON data to SOCKET.")
  (:method (command-string json socket)
    (declare (ignore command-string json socket))
    nil))
(defun irc-reply (message json socket)
  "Send MESSAGE as a reply back over fbi."
  (declare (type string message)
           (type json-socket socket)
           (type publish json))
  (write-json
   (make-irc-private-message (from json) (server json) (channel json)
                             message)
              socket :force t))

(defun normalize-irc-command (command-string)
  "Lowercase the COMMAND-STRING for method selection."
  (declare (type string))
  (string-downcase command-string))

(defgeneric action-hook (json-action-mixin socket))

(defmethod action-hook ((json json-action-mixin) (sock json-socket))
  "Ignore actions we don't know of and return nil."
  (declare (ignore json sock))
  nil)

(defmethod action-hook ((json publish) (sock json-socket))
  "Publish means lots of things, dispatch JSON for SOCK as needed."
  (fbi-message-hook (make-keyword (from json)) json sock))

(defgeneric fbi-message-hook (from json-mixin json-socket)
  (:documentation 
   "If we don't know what FROM is, don't handle it, but don't error.")
  (:method ((from symbol) (json json-mixin) (sock json-socket))
    nil))

(defmethod fbi-message-hook ((from (eql :irc)) (json publish) (sock json-socket))
  (irc-command-hook (make-keyword (command json)) json sock))

(defun listen-loop (socket)
  "Listen for input and send it through nisp."
  (iter (for sock = (wait-for-input socket :timeout 36000 :ready-only t))
        (when sock
          (action-hook (read-json (car sock)) (car sock)))))


(defmethod irc-command-hook ((cmd (eql :hi)) json socket)
  (write-json (make-irc-private-message "irc" (server json)
                            (channel json)
                            "nice to meet you!")
              socket :force t))

;;;; End of file