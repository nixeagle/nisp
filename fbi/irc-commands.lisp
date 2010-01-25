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

(defun normalize-irc-command (command-string)
  "Lowercase the COMMAND-STRING for method selection."
  (declare (type string))
  (string-downcase command-string))

(defgeneric action-hook (json-action-mixin socket))

(defmethod action-hook ((json json-action-mixin) (sock json-socket))
  "Ignore actions we don't know of and return nil."
  (declare (ignore json sock))
  nil)

(defun handle-irc-command (json socket)
  (irc-command-hook (intern (string-upcase (command json)) :keyword) json socket))

(defun listen-loop (socket)
  "Listen for input and send it through nisp."
  (iter (for sock = (wait-for-input socket :timeout 36000 :ready-only t))
        (when sock
          (json-nisp-message (nstring-downcase
                              (princ-to-string
                               (json->alist (read-json (car sock)))))))))


(defmethod irc-command-hook ((cmd (eql :hi)) json socket)
  (write-json (make-irc-private-message "irc" (server json)
                            (channel json)
                            "nice to meet you!")
              socket :force t))

;;;; End of file