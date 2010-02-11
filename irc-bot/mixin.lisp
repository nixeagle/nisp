;;; General mixins
(in-package :nisp.i)

(defclass identity-mixin () ()
  (:documentation "Direct mapping to person's identity.

This is different then any handle/nick/username/email. A person's
identity should be the same across whatever protocols we operate
over."))

