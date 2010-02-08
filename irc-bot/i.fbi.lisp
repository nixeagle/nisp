(defpackage #:nisp.json-utils
  (:use :cl :json :alexandria :usocket :eos :with-fbound)
  (:export :json-socket :json-mixin))
(in-package :nisp.json-utils)

(defclass json-socket (usocket:stream-usocket) ()
  (:documentation "Socket specifically for json operations."))


(defpackage #:nisp.i.fbi
  (:use :cl :iterate :usocket :alexandria :with-fbound))

(in-package :nisp.i.fbi)


;danopia.net 5348

