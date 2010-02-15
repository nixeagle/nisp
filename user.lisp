;;;; Package for including all sorts of interesting and useful libraries
;;;; for testing.
(in-package :cl-user)

(defpackage #:nisp.user
  (:use :cl
        ;; implentation independent sockets
        :usocket
        :drakma)                         ;http-request
  (:nicknames :nu))

(in-package :nisp.user)
;;; In custom userland, this userland has modifications from a standard
;;; userland.
