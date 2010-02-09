(defpackage #:nisp.util.usocket
  (:use :cl :usocket)
  (:export :read-ready-p)
  (:documentation "Helpers for the usocket library."))
(in-package :nisp.util.usocket)

(defgeneric read-ready-p (socket)
  (:documentation "Return non-nil when socket is ready to read from."))

(defmethod read-ready-p ((sock usocket))
  (listen (socket-stream sock)))