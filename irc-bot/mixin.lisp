;;; General mixins
(in-package :nisp.i)

(defclass identity-mixin () ()
  (:documentation "Direct mapping to person's identity.

This is different then any handle/nick/username/email. A person's
identity should be the same across whatever protocols we operate
over."))


(defclass connection-mixin () ()
  (:documentation "Protocol agnostic interface specification."))

(defclass sender-mixin () ())

(defclass target-mixin () ())

(defclass message-mixin () ())


;;; END