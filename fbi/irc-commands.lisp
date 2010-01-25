(defpackage #:nisp.fbi.irc-commands
  (:use :cl :usocket :iterate :json :nisp.util-protocol
        :nisp.util-types
        :bordeaux-threads
        :nisp.fbi.json-classes
        :nisp.fbi.sockets))

(in-package :nisp.fbi.irc-commands)

(defun listen-loop (socket)
  "Listen for input and send it through nisp."
  ;; Broken atm
  (iter (for sock = (wait-for-input socket :ready-only t))
        (json-nisp-message (nstring-downcase
                            (princ-to-string
                             (json->alist (read-json sock)))))))
