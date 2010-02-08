(defpackage #:nisp.util.json
  (:use :cl :json :usocket
        :eos :with-fbound)
  (:export :json-socket :json-mixin :json-socket-connect))
(in-package :nisp.util.json)

(defclass json-mixin () ()
  (:documentation "Any class that presents a json interface."))

(defclass json-socket (usocket:stream-usocket) ()
  (:documentation "Socket specifically for json operations."))

(defun json-socket-connect (host port)
  "Connect to HOST on PORT with a `json-socket'."
  (change-class (usocket:socket-connect host port)
                'json-socket))

(def-suite root)
(test (json-socket-connect :suite root)
  (let ((sock (json-socket-connect "danopia.net" 5348)))
    (is (typep sock 'json-socket))
    (is (close (socket-stream sock)))))