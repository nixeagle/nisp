(defpackage #:nisp.9p
  (:use :cl :alexandria :iterate :eos :with-fbound)
  (:nicknames :9p))

(in-package :nisp.9p)


(defun %make-local-stream-socket ()
  "Make a local unix socket."
  (make-instance 'sb-bsd-sockets:local-socket :type :stream))

(defparameter *sock* (%make-local-stream-socket))


(defparameter *1* (sb-bsd-sockets:socket-connect *sock* "/tmp/ns.james.:0/wmii"))

(defstruct (tb (:copier nil)
               (:predicate nil)
               (:type vector)
               #+ () :named)
  (size #(0 0 0 0) :type (vector unsigned-byte 4))
  (type 0 :type unsigned-byte)
  (tag #(0 0) :type (vector unsigned-byte 2)))

(defmethod tb ((tb tb))
  (describe tb))

(deftype 9p-byte ()
  '(unsigned-byte 1))

#+ ()
;;; These enumerations are defined in fcall.h
(macrolet ((define-type (name number)
             `(defclass ,(alexandria:format-symbol t "~A-message" name) ()
                ()
                (:default-initargs :type ,number))))
  (define-type openfd 98)
  (define-type version 100)
  (define-type auth 102)
  (define-type attach 104)
  (define-type error 106)
  (define-type flush 108)
  (define-type walk 110)
  (define-type open 112)
  (define-type create 114)
  (define-type read 116)
  (define-type write 118)
  (define-type clunk 120)
  (define-type remove 122)
  (define-type stat 124)
  (define-type wstat 126)
  (define-type max 128))

(in-package :9p)

(defclass client-message ()
  ()
  (:documentation "Message is sent by a client."))
(defclass server-message ()
  ()
  (:documentation "Message is sent by a server in reply to a client."))
(defclass no-tag ()
  ()
  (:documentation "No tag on a message means its 0.")
  (:default-initargs :tag 0))

(defclass file-id ()
  ((file-id :type '(unsigned-byte 32)
            :documentation "Identifies a current file on the server.")))

(defclass message ()
  ((size :documentation "Length in bytes of the entire message.")
   (type :initarg :type :documentation "Single byte for a message type.")
   (tag :documentation "Identifying tag.")))

(defun 9p-version-string-p (string)
  "True if STRING begins with \"9P\"."
  (and (stringp string)
       (< 1 (length string))
       (string= "9P" string :end2 2)))

(deftype 9p-version-string ()
  "Must be a string and start with \"9P\"."
  `(and string (satisfies 9p-version-string-p)))

(defclass version-message (message no-tag)
  ((message-size-limit :documentation "Largest message ever expected to handle.")
   (version :type 9p-version-string :initarg :version))
  (:default-initargs :type 100))

(defclass 9P2000-version-message (message)
  ()
  (:default-initargs :version "9P2000b"))