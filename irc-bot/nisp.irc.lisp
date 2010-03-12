(in-package :cl-user)
(defpackage #:nisp.irc
  (:use :common-lisp :nisp.clos.maximum-length
        :iterate :metabang-bind :nisp.irc-types
        ))
(in-package :nisp.irc)
;;; General generics

(defgeneric convert->string (object)
  ;; Methods specializing on this generic may cease to exist at any moment.
  ;; This exists for prototyping only atm.
  ;;
  ;; An interesting thought is to have this or some other method become
  ;; a setf'able place such that (setf (convert->string OBJECT) "some
  ;; string") would correctly store the contents of "some string" in
  ;; OBJECT.
  (:documentation "Convert OBJECT to a string representation.
This is completely unrelated to the lisp reader."))

;;; Mixins

(defclass username-mixin (limited-length-mixin) ())
(defclass host-mixin (limited-length-mixin) ())
(defclass nickname-mixin (limited-length-mixin) ())
(defclass identifier-mixin (username-mixin host-mixin nickname-mixin) ())
(defclass channel-mixin () ())
(defclass mode-mixin () ())
(defclass message-mixin (limited-length-mixin) ())

(defgeneric username (object)
  (:method ((object username-mixin))
    (error "Method on mixin class invalid.")))
(defgeneric nickname (object)
  (:method ((object nickname-mixin))
    (error "Method on mixin class invalid.")))
(defgeneric normalize-nickname (object)
  (:method ((object nickname-mixin))
    (error "Method on mixin class invalid.")))
(defgeneric host (object)
  (:method ((object host-mixin))
    (error "Method on mixin class invalid.")))
(defgeneric (setf nickname) (nick object)
  (:documentation
   "Set NICK on OBJECT.

NICK has several constraints.
  - It must be a character string.
  - It must start with NICKNAME-START-CHARACTER.
  - Type of the second character on must be NICKNAME-CHARACTER.
  - Its length must pass VALID-LENGTH-P.  This uses the MAXIMUM-LENGTH
    slot on OBJECT."))



(defclass maximum-message-length (maximum-length)
  ((maximum-length :type maximum-message-size))
  (:default-initargs :maximum-length 512))

(defclass abstract-username (username-mixin maximum-length)
  ((user :type string
         :accessor username
         :initarg :user
         :initform (error "Username must be provided."))))

(defclass abstract-nickname (nickname-mixin maximum-length)
  ((nickname :type string
             :accessor nickname
             :initarg :nick
             :initform (error "Nickname must be provided.")
             :documentation "IRC user nickname")))

(defclass abstract-host (host-mixin maximum-length)
  ((host :initarg :host
         :accessor host)))

(defclass nickname (abstract-nickname maximum-message-length)
  ((nickname :type nickname-string)))
(defclass username (abstract-username maximum-message-length)
  ((username :type username-string)))
(defclass host (abstract-host maximum-message-length)
  ((host :type string)))                ;should be more specific...

(defclass identifier (identifier-mixin)
  ((user :type username)
   (nickname :type nickname)
   (host :type host)))

(defmethod convert->string ((object username-mixin))
  (username object))
(defmethod convert->string ((object nickname-mixin))
  (nickname object))
(defmethod convert->string ((object host-mixin))
  (host object))

(defmethod convert->string ((object identifier-mixin))
  (format nil "~A!~A@~A"
          (nickname (nickname object))
          (username (username object))
          (host (host object))))

(defmethod valid-length-p ((object limited-length-mixin))
  (length<= (convert->string object) (maximum-length object)))

(defclass rfc-nickname (nickname maximum-message-length)
  ((nickname :type nickname-string))
  (:default-initargs :maximum-length 9)) ;Based on rfc2812

(defmethod normalize-nickname ((object nickname-mixin))
  "Lowercase all ASCII letters in OBJECT.

This does _not_ cause [ ] \\ ~ to be translated to { } | ^."
  (normalize-nickname (nickname object)))

(defmethod normalize-nickname ((nickname string))
  (string-downcase nickname))

(defmethod (setf nickname) ((nickname string) (object nickname))
  (assert (valid-length-p object))
  (setf (slot-value object 'nickname) nickname))

(defclass rfc-username (username maximum-message-length)
  ((user :type username-string))
  (:default-initargs :maximum-length 30)) ;Not correct, works for now


(defclass rfc-host (host) ())

(defclass host-address (host) ())

(defclass ipv4-host (host-address)
  ((host :type ipv4-address-string))
  (:documentation "Your normal host. 127.0.0.1 and so on."))

(defclass ipv6-host (host-address) ())

(defclass host-name (host)
  ((host :type string)))

(defclass rfc-host-name (host-name)
  ((host :documentation "Host with very small charset: [a-zA-Z0-9\\\-\\\.]."))
  (:documentation "More restricted form of hostname, specified in rfc1123."))

(defclass rfc-identifier (identifier)
  ((user :type rfc-username)
   (nickname :type rfc-nickname)
   (host :type rfc-host))
  (:documentation "Represents nickname!user@host"))

(defun make-username (username &rest initargs &key &allow-other-keys)
  "Make username instance unless USERNAME is of type abstract-username."
  (if (typep username 'rfc-username)
      username
      (apply #'make-instance 'rfc-username :username username initargs)))

(defun make-nickname (nickname &rest initargs &key &allow-other-keys)
  "Make nickname instance unless NICKNAME is of type abstract-nickname."
  (if (typep nickname 'nickname)
      nickname
      (apply #'make-instance 'rfc-nickname :nickname nickname initargs)))

(defun make-host (host &rest initargs &key &allow-other-keys)
  "Make host instance unless HOST is of type abstract-host."
  (if (typep host 'host)
      host
      (apply #'make-instance 'rfc-host :host host initargs)))

#+ () (defmethod initialize-instance ((instance identifier)
                                &rest initargs &key &allow-other-keys)
  (setf (getf initargs :username)
        (make-username (getf initargs :username)))
  (setf (getf initargs :nickname)
        (make-nickname (getf initargs :nickname)))
  (setf (getf initargs :host)
        (make-host (getf initargs :host)))
  (apply #'call-next-method instance initargs))

(defclass channel () ())
(defclass mode () ())
(defclass message (message-mixin limited-length-mixin) ())
(defclass irc-message (message maximum-message-length) ())


;;; protocol stuff

(defclass raw-protocol-line-mixin ()
  ()
  ;; needs a way to measure length... for type info.
  )

(defclass raw-protocol-line (raw-protocol-line-mixin limited-length-mixin)
  ((raw-protocol-line :accessor raw-protocol-line)))

(defclass raw-irc-protocol-line (raw-protocol-line maximum-message-length) ())

(defpackage #:nisp-irc-user
  (:use :cl :usocket :nisp.irc :nisp.irc-types :alexandria
        :iterate))
(in-package :nisp-irc-user)
