(in-package :nisp.i)

(defclass bot-connection (connection comchar
                                     abstract-data-source
                                     abstract-data-sink) ()
  (:default-initargs :username "lisp" :nickname "nisp"
                     :realname "Nixeagle's lisp experiments"
                     :server-port 6667
                     :comchar ","))
(defclass connect-with-background-handler-mixin () ()
  (:documentation "Superclass this to connect directly to background on
methods that support this."))

(defmethod shared-initialize :after ((bot bot-connection) (slot-names t)
                                     &key nickname username realname)
  (when (and nickname username realname)
    (setf (slot-value bot 'irc:user)
          (make-instance 'bot-user
                         :nickname nickname
                         :username username
                         :realname realname))))

(defgeneric connect (connection &key &allow-other-keys))
(defmethod connect :before ((bot bot-connection) &key ssl)
  (setf (slot-value bot 'irc::socket)
        (usocket:socket-connect (irc:server-name bot)
                                (irc:server-port bot)
                                :element-type 'flexi-streams:octet))
  (setf (irc:network-stream bot) (if ssl
                                     (funcall #'cl+ssl:make-ssl-client-stream
                                              (usocket:socket-stream
                                               (irc::socket bot)))
                                     (usocket:socket-stream
                                      (irc::socket bot))))
  (setf (irc:output-stream bot) (flexi-streams:make-flexi-stream
                                 (irc:network-stream bot)
                                 :element-type 'character
                                 :external-format :UTF8)))

(defmethod connect ((bot connection) &key mode)
  (when (irc:password bot)
    (irc:pass bot (irc:password bot)))
  (irc:nick bot (nickname bot))
  (irc:user- bot (or (username bot) (nickname bot))
             (or mode 0) (or (realname bot) (nickname bot)))
  bot)

(defmethod connect :after ((bot connection) &key)
  (irc:add-default-hooks bot))

(defmethod compute-connection-id ((irc connection))
  "Uniquely identify IRC connections by `nickname'@`server-name'."
  (concatenate 'string (irc:nickname irc) "@" (irc:server-name irc)))

(defun nisp-start-read-loop (irc)
  (describe irc)
  (irc:read-message-loop irc))


(defmethod connect :after ((irc connect-with-background-handler-mixin) &key)
  "Start IRC's command loop in a different thread."
  (bordeaux-threads:make-thread (lambda ()
                                  (nisp-start-read-loop irc))
                                :name (compute-connection-id irc)))

(defmethod connect :after ((irc bot-connection) &key)
  "Make sure IRC's hook is setup."
  (irc:add-hook irc 'irc:irc-privmsg-message 'irc-handle-privmsg)
  (sleep 2))
