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

;;; Next 5 not well specified atm, will be working on this
;;; soon. 01-03-2010
(defclass abstract-from () ()
  (:documentation "Generic information about a sender."))

(defclass abstract-to () ()
  (:documentation "Generic information about a recipient/target/channel"))

(defclass abstract-medium () ()
  (:documentation "Protocol/service information."))

(defclass abstract-data-source () ()
  (:documentation "Generic information on the _source_ of a message.

This is different from `abstract-from' in that here we care about how we
got the message, not who or what sent it."))

(defclass abstract-data-sink () ()
  (:documentation "Generic information about destination protocol/service.

This is different from `abstract-to' in that here we care about where a
  message is routed to, as opposed to who or what is to eventually receive
  the message. The term 'sink' means what medium a message will be
  convoyed over to reach its destination as specified by `abstract-to'."))

(defclass abstract-action () ()
  (:documentation "Generic information about intended actions.

This will vary widely depending on source/sink combinations, but we need
this for generic function/default method definitions as well as interface
specification."))

(defclass abstract-message-content () ()
  (:documentation "Message contents/information"))

;;; possibly include abstract-message as a composed class or somehow
;;; holding all these parts in one interface, which is why we rename
;;; abstract-message to abstract-message-content.

;;; END