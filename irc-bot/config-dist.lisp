(in-package :nisp.i)


;;; Connection details here.
(defclass main-irc-bot (bot-connection
                               connect-with-background-handler-mixin)
  ()
  (:default-initargs :server-name "irc.ninthbit.net"
    :comchar #\!
    :nickname "nisp-dist")
  (:documentation "Demonstration class

Shows how to setup a connection to ninthbit.net using a comchar of !."))

;;; Tell the bot what to do after it connects. For now this includes
;;; channel joins and talking to nickserv.
(defmethod connect :after ((irc main-irc-bot) &key)
  (irc:join irc "#bots")
  (irc:privmsg irc "#bots" "hi all!"))

(defvar *main-irc-bot* (make-instance 'main-irc-bot))

(unless (connectedp *main-irc-bot*) (connect *main-irc-bot*))
