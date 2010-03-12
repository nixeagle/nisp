(eval-when (:compile-toplevel)
  (asdf:load-system :cl-json))
(defpackage #:nisp.fbi
  (:use :cl :usocket :json :iterate :nisp.util.json))

(in-package :nisp.fbi)

;(delete-package :nisp.fbi.json-classes)


(defpackage #:nisp.fbi.sockets
  (:use :cl :usocket :json :iterate
        :nisp.fbi.json-classes :nisp.util.json :nisp.util.usocket)
  (:nicknames :fbi-sockets)
  (:shadow :socket-connect))

(in-package :nisp.fbi.sockets)



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
