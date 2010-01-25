(defpackage #:nisp.fbi.irc-commands
  (:use :cl :usocket :iterate :json :nisp.util-protocol
        :nisp.util-types
        :bordeaux-threads
        :nisp.fbi.json-classes
        :nisp.fbi.sockets))

(in-package :nisp.fbi.irc-commands)

(defun normalize-irc-command (command-string)
  "Lowercase the COMMAND-STRING for method selection."
  (declare (type string))
  (string-downcase command-string))

(defun listen-loop (socket)
  "Listen for input and send it through nisp."
  ;; Broken atm
  (iter (for sock = (wait-for-input socket :timeout 36000 :ready-only t))
        (when sock
          (json-nisp-message (nstring-downcase
                              (princ-to-string
                               (json->alist (read-json (car sock)))))))))

(sb-bsd-sockets:local-abstract-socket
 )