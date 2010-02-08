(in-package :nisp.i)

(defun split-command-string (command-string)
  "Split COMMAND-STRING into appropriate parts."
  (declare (type string command-string))
  (split-sequence #\Space command-string :remove-empty-subseqs t))

(test (split-command-string :suite command-routing)
  (with-fbound (split-command-string)
    "Expect the string with no spaces in a list."
    ("Hi") '("Hi")
    "Expect a list with the 4 words as elements."
    ("Hi how are you") '("Hi" "how" "are" "you") 
    (" Hi how are you") '("Hi" "how" "are" "you")
    ("Hi how are you ") '("Hi" "how" "are" "you")))

;;; not bot related directly...
(defun add-superclass (instance new-superclass)
  "Add NEW-SUPERCLASS to INSTANCE's superclass list."
  (reinitialize-instance (class-of instance) :direct-superclasses
                         (adjoin (find-class new-superclass)
                                  (c2cl:class-direct-superclasses
                                   (class-of instance)))))

(defun delete-superclass-of (instance superclass)
  "Delete SUPERCLASS from INSTANCE's superclass list."
  (reinitialize-instance (class-of instance) :direct-superclasses
                         (remove (find-class superclass)
                                 (c2cl:class-direct-superclasses
                                  (class-of instance)))))

(defun make-anon-bot-user-class (user-instance)
  "Make brand new anon class for USER-INSTANCE."
  (make-instance 'standard-class :direct-superclasses
                 (list (find-class 'bot-user)) :name
                 `(,(nickname user-instance) ,(irc:hostname user-instance))))

(defun change-user-class-to-bot-user (user-instance)
  "Change class of USER-INSTANCE to anon class."
  (change-class user-instance (make-anon-bot-user-class user-instance)))


(defvar *irc-bot-instances* nil
  "Global list of bot instances.")

(defclass bot-user (user) ()
  (:documentation "This does not bother with normalization.

All things made by `make-anon-bot-user-class' superclass this."))

(defclass connection (irc:connection) ())
(defclass bot-connection (connection comchar) ())
(defclass connect-with-background-handler-mixin () ()
  (:documentation "Superclass this to connect directly to background on
methods that support this."))

(defclass 8b-i-bot-connection (bot-connection
                               connect-with-background-handler-mixin)
  ()
  (:default-initargs :username "lisp" :nickname "i"
                     :realname "bot" :server-port 6667
                     :server-name "irc.eighthbit.net"
                     :comchar ","))

(defclass bot-admin () ())

(defmethod shared-initialize :after ((bot bot-connection) slot-names
                                     &key nickname username realname)
  (when (and nickname username realname)
    (setf (slot-value bot 'user)
          (make-instance 'bot-user
                         :nickname nickname 
                         :username username
                         :realname realname))))

(defmethod nickname ((irc connection))
  "Return CONNECTION's nickname."
  (nickname (user irc)))

(defmethod username ((irc connection))
  "Return CONNECTION's username."
  (username (user irc)))

(defmethod realname ((irc connection))
  "Return CONNECTION's realname."
  (realname (user irc)))

(defmethod irc:privmsg ((irc irc:connection) target message)
  "Turn MESSAGE into a string before sending through CONNECTION to TARGET."
  (irc:privmsg irc target (princ-to-string message)))

(defgeneric message-text (object))
(defmethod message-text ((msg irc:irc-message))
  "Return the string containing the irc-message."
  (first (last (irc:arguments msg))))

(defmethod find-channel-user ((connection connection)
                              (channel irc:channel) (nickname string))
  "Find NICKNAME in CHANNEL on CONNECTION."
  (let ((nickname (irc:normalize-nickname connection nickname)))
    (or (gethash nickname (irc:users channel))
        (when (string= nickname (nickname (user connection)))
          (user connection)))))

;;; Method written with heavy cribbing from cl-irc in command.lisp.
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

(defmethod connect :after ((irc connect-with-background-handler-mixin) &key)
  "Start IRC's command loop in a different thread."
  (irc:start-background-message-handler irc))

(defmethod target ((message irc:irc-privmsg-message))
  "String with message target."
  (car (irc:arguments message)))

(defvar *it*)
(defmethod test-handle-privmsg ((message irc:irc-privmsg-message))
  "i bot test."
  (when (and (string= (target message) "#bots")
             )
    (setq *it* message)
    (handle-command (irc:connection message)
                    message
                    (target message)
                    (message-text message))))

(defclass sender ()
  ((host :accessor host :initarg :host)
   (source :accessor source :initarg :source)
   (user :accessor user :initarg :user)))

(defmethod ensure-user-host ((user irc:user) (host string))
  (when (string= (irc:hostname user) "")
    (setf (irc:hostname user) host))
  user)

(defgeneric add-irc-user-superclass (connection user superclass)
  (:documentation "Add SUPERCLASS to USER on CONNECTION."))

(defmethod add-irc-user-superclass ((irc connection) (user bot-user)
                                    (superclass symbol))
  (declare (ignore irc))
  (add-superclass user superclass))
(defmethod add-irc-user-superclass ((irc connection) (user irc:user)
                                    (superclass symbol))
  "Change USER's class and recall."
  (add-irc-user-superclass irc
                           (change-user-class-to-bot-user user)
                           superclass))
(defmethod add-irc-user-superclass ((irc connection) (nickname string)
                                    (superclass symbol))
  "Find NICKNAME in CONNECTION and recall."
  (add-irc-user-superclass irc
                           (irc:find-user irc nickname)
                           superclass))

(defgeneric handle-command (connection sender to cmd))

(defmethod handle-command ((irc connection) sender (to string) cmd)
  "Look up which channel TO is talking about and pass it along."
  (handle-command irc sender (irc:find-channel irc to) cmd)
  #+ () (irc:privmsg connection to connection))

(defmethod handle-command ((irc connection) sender (to irc:channel) cmd)
  (handle-command irc
                  (ensure-user-host (find-channel-user irc to (irc:source sender))
                                    (irc:host sender))
                  to cmd))


(defmethod handle-command ((irc connection) (sender irc:user)
                           (to irc:channel) cmd)
  (describe sender))

(defmethod handle-command ((irc connection) (sender bot-admin)
                           (to irc:channel) cmd)
  (irc:privmsg irc to cmd))


;;; Types
