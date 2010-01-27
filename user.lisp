;;;; Package for including all sorts of interesting and useful libraries
;;;; for testing.
(in-package :cl-user)
(defpackage #:nisp.user
  (:use :cl :iterate
        ;; implentation independent sockets
        :usocket
        ;; Nisp related types
        :nisp.irc :nisp.irc-types :nisp.util-types
        
        ;; General nisp related short packages
        :nisp.clos.maximum-length       ; Concept of restricted length.
        )
  (:nicknames :nu))

(in-package :nisp.user)
;;; In custom userland, this userland has modifications from a standard
;;; userland.