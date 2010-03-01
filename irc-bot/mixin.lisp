;;; General mixins
(in-package :nisp.i)

(defclass identity-mixin () ()
  (:documentation "Direct mapping to person's identity.

This is different then any handle/nick/username/email. A person's
identity should be the same across whatever protocols we operate
over."))


(defclass connection-mixin () ()
  (:documentation "Protocol agnostic interface specification.

In principle any connection class derived from usocket that implements all
methods specialized on this object is a complient interface."))

;;; Next 3 not well specified atm, will be working on this
;;; soon. 01-03-2010
(defclass from-mixin () ()
  (:documentation "Generic information about a sender."))

(defclass to-mixin () ()
  (:documentation "Generic information about a recipient/target/channel"))

(defclass message-mixin () ()
  (:documentation "Message contents/information"))



;;; OBSOLETE - replaced by from-mixin and to-mixin
(defclass sender-mixin (from-mixin) ())
(defclass target-mixin (to-mixin) ())




;;; END