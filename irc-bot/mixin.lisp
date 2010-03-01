;;; General mixins
(in-package :nisp.i)

(defclass abstract-identity () ()
  (:documentation "Direct mapping to person's identity.

This is different then any handle/nick/username/email. A person's
identity should be the same across whatever protocols we operate
over."))


(defclass abstract-connection () ()
  (:documentation "Protocol agnostic interface specification.

In principle any connection class derived from usocket that implements all
methods specialized on this object is a complient interface."))

;;; Next 3 not well specified atm, will be working on this
;;; soon. 01-03-2010
(defclass abstract-from () ()
  (:documentation "Generic information about a sender."))

(defclass abstract-to () ()
  (:documentation "Generic information about a recipient/target/channel"))

(defclass abstract-message-content () ()
  (:documentation "Message contents/information"))

;;; possibly include abstract-message as a composed class or somehow
;;; holding all these parts in one interface, which is why we rename
;;; abstract-message to abstract-message-content.

;;; END