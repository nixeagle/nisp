(defpackage #:nisp.fbi.irc-commands
  (:use :cl :usocket :iterate :json :nisp.util-protocol
        :nisp.fbi.util
        :nisp.util-types
        :bordeaux-threads
        :nisp.fbi.json-classes
        :nisp.fbi.sockets))

(in-package :nisp.fbi.irc-commands)


(defgeneric irc-command-hook (command-string json socket)
  (:documentation "Act on COMMAND-STRING with JSON data to SOCKET.")
  (:method (command-string json socket)
    (declare (ignore command-string json socket))
    nil))
(defgeneric action-hook (json-action-mixin socket)
  (:documentation "All FBI recieved actions get sent here. ~

                   Specializations here are based on the type of action recieved ~
                   and "))
(defgeneric fbi-message-hook (from json-mixin json-socket)
  (:documentation 
   "Messages that are FROM a specific component get directed here.")
  (:method ((from symbol) (json json-mixin) (sock json-socket))
    "If we don't know what FROM is, don't handle it, but don't error."
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

(defmethod action-hook ((json json-action-mixin) (sock json-socket))
  "Ignore actions we don't know of and return nil."
  (declare (ignore json sock))
  nil)

(defmethod action-hook ((json publish) (sock json-socket))
  "Publish means lots of things, dispatch JSON for SOCK as needed."
  (fbi-message-hook (make-keyword (from json)) json sock))

(defmethod fbi-message-hook ((from (eql :irc)) (json publish) (sock json-socket))
  (irc-command-hook (make-keyword (command json)) json sock))

(defmethod fbi-message-hook ((from (eql :udp)) (json publish) (sock json-socket))
  (nisp.fbi.json-classes:json-nisp-message (string-downcase (json->alist json))))

(defun start-command-loop (socket)
  "Spawn a new thread listening for commands on SOCKET."
  (declare (type json-socket socket))
  (make-thread (lambda () (listen-loop socket)) :name :nisp.fbi-command-loop))

(defun listen-loop (socket)
  "Listen for input and send it through nisp."
  (iter (for sock = (wait-for-input socket :timeout 36000 :ready-only t))
        (when sock
          (action-hook (read-json (car sock)) (car sock)))))

(defmethod irc-command-hook :around (cmd json socket)
  (irc-reply (call-next-method) json socket))

(defmethod irc-command-hook ((cmd (eql :hello)) json socket)
  "Say hello in many languages... just as nisp does ,(hello)."
  (nisp.hello:hello))

;;;; End of file