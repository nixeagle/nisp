(defpackage #:nisp.9p
  (:use :cl)
  (:nicknames :9p))

(in-package :nisp.9p)


(defun %make-local-stream-socket ()
  "Make a local unix socket."
  (make-instance 'sb-bsd-sockets:local-socket :type :stream))

(defparameter *sock* (%make-local-stream-socket))


(defparameter *1* (sb-bsd-sockets:socket-connect *sock* "/tmp/ns.james.:0/wmii"))
