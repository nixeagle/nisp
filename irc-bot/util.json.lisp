(defpackage #:nisp.util.json
  (:use :cl :json :usocket :nisp.util.usocket
        :eos :with-fbound)
  (:export :json-socket :json-mixin :json-socket-connect
           :make-json-mixin-from-string
           :read-json :write-json :synchronous-json-request))
(in-package :nisp.util.json)

(defclass json-mixin () ()
  (:documentation "Any class that presents a json interface."))

(defclass json-socket (usocket:stream-usocket) ()
  (:documentation "Socket specifically for json operations."))


(defun make-json-mixin-from-string (string)
  ;; Not positive if this needs to stay or not...
  (declare (type string string))
  (let ((json:*prototype-name* 'hash-type)
        (json:*json-symbols-package* :nisp.fbi.json-classes))
    (json:with-decoder-simple-clos-semantics
      (json:decode-json-from-string
       string))))

(defun json-socket-connect (host port)
  "Connect to HOST on PORT with a `json-socket'."
  (change-class (usocket:socket-connect host port)
                'json-socket))

(defgeneric write-json (json-mixin json-socket &key force)
  (:documentation "Write json to JSON-SOCKET."))
(defgeneric read-json (json-socket))

(defmethod write-json :around (json-mixin json-socket &key force)
  "Default FORCE to true."
  (print force)
  (call-next-method json-mixin json-socket :force (not force)))
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

(defun synchronous-json-request (json sock &key
                            (read-function 'read-json)
                            (write-function 'write-json))
  "Send JSON to SOCK and wait for a reply."
  (funcall write-function json sock)
  (wait-for-input sock)
  (funcall read-function sock))

(def-suite root)
(test (json-socket-connect :suite root)
  (let ((sock (json-socket-connect "danopia.net" 5348)))
    (is (typep sock 'json-socket))
    (is (close (socket-stream sock)))))

