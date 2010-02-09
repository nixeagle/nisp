(eval-when (:compile-toplevel)
  (asdf:load-system :cl-json))
(defpackage #:nisp.fbi
  (:use :cl :usocket :json :iterate :nisp.util-protocol :nisp.util.json))

(in-package :nisp.fbi)

;(delete-package :nisp.fbi.json-classes)


(defpackage #:nisp.fbi.sockets
  (:use :cl :usocket :json :iterate :nisp.util-protocol
        :nisp.fbi.json-classes :nisp.util.json :nisp.util.usocket)
  (:nicknames :fbi-sockets)
  (:export #:write-json
           #:read-json)
  (:shadow :socket-connect))

(in-package :nisp.fbi.sockets)

(defgeneric write-json (json-mixin json-socket &key force)
  (:documentation "Write json to JSON-SOCKET."))
(defgeneric read-json (json-socket))

(defmethod write-json ((json-object json-mixin) (sock json-socket)
                       &key force)
  "Write and optionally FORCE JSON-OBJECT to JSON-SOCKET."
  (prog1
      (encode-json json-object (socket-stream sock))
    (terpri (socket-stream sock))
    (and force (force-output (socket-stream sock)))))

(defmethod read-json ((sock json-socket))
  "Read from JSON-SOCKET returning a `JSON-MIXIN'."
  (and (read-ready-p sock)
       (make-json-mixin-from-string (read-line (socket-stream sock)))))


;;; Not sure what these are, messages, commands, part of json-classes...
;;; Going here for now.
(defun authenticate-component (json-socket component password)
  "Authenticate to JSON-SOCKET as COMPONENT with PASSWORD."
  (declare (type string component password))
  (write-json (make-instance 'auth
                             :user component
                             :secret password)
              json-socket :force t))

;;; End fbi.lisp
