(eval-when (:compile-toplevel)
  (asdf:load-system :cl-json))
(defpackage #:nisp.fbi
  (:use :cl :usocket :json :iterate :nisp.util-protocol))

(in-package :nisp.fbi)

;(delete-package :nisp.fbi.json-classes)
(defpackage #:nisp.fbi.json-classes
  (:use :cl :usocket :json :iterate :nisp.util-protocol)
  (:export :auth :subscribe :json-mixin :json-action-mixin))
(in-package :nisp.fbi.json-classes)

(defun make-json-type-signature (alist)
  "Return a list for use as a type signature.

A type signature is basically a list of all keys in a hash table from cl-json"
  (mapcar #'car alist))
(defclass json-mixin () ())

(defclass json-action-mixin (json-mixin)
  ((action :initform (error "Action must be provided or FBI fails silently.")
           :type string)))

(defclass auth (json-action-mixin)
  ((action :initform "auth")
   (user :accessor user
         :initarg :user)
   (secret :accessor secret
           :initarg :secret))
  (:documentation "Auth with FBI."))

(defclass subscribe (json-action-mixin)
  ((action :initform "subscribe")
   (channels :accessor channels
             :initarg :channels))
  (:documentation "Subscribe to FBI channels."))

(defpackage #:nisp.fbi.sockets
  (:use :cl :usocket :json :iterate :nisp.util-protocol
        :nisp.fbi.json-classes)
  (:shadow :socket-connect))

(in-package :nisp.fbi.sockets)

(defclass json-socket (usocket:stream-usocket) ())
(defgeneric read-ready-p (socket)
  (:documentation "Return non-nil if socket is ready to read from."))
(defgeneric write-json (json-mixin json-socket &key force)
  (:documentation "Write json to JSON-SOCKET."))
(defgeneric read-json (json-socket))

(defmethod read-ready-p ((sock json-socket))
  (listen (socket-stream sock)))

(defun json-socket-connect (host port)
  (change-class (usocket:socket-connect host port)
                'json-socket))

(defmethod write-json ((json-object json-mixin) (sock json-socket)
                       &key force)
  "Write and optionally FORCE JSON-OBJECT to JSON-SOCKET."
  (prog1
      (encode-json json-object (socket-stream sock))
    (terpri (socket-stream sock))
    (and force (force-output (socket-stream sock)))))

(defun make-json-mixin-from-string (string)
  (declare (type string string))
  (let ((json:*prototype-name* 'action)
        (json:*json-symbols-package* :nisp.fbi.json-classes))
    (json:with-decoder-simple-clos-semantics
      (json:decode-json-from-string
       string))))

(defmethod read-json ((sock json-socket))
  "Read from JSON-SOCKET returning a `JSON-MIXIN'."
  (and (read-ready-p sock)
       (make-json-mixin-from-string (read-line (socket-stream sock)))))

(defmethod make-object :around (bindings (symbol symbol) &optional superclasses)
  (if (or (null symbol) (find-class symbol nil))
      (call-next-method)
      (make-object bindings nil superclasses)))