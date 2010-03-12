(in-package :nisp.i)

(defvar +root-directory+
  (asdf:system-relative-pathname (asdf:find-system :nisp.i) "/")
  "The root of where the asdf source is at.

We use this for locating data and configuration information for
nisp.i. This may run nto some issues in the future but for the near term
future this solves most issues.")

(defclass connection (irc:connection) ())

(defun make-anon-bot-user-class (user-instance)
  "Make brand new anon class for USER-INSTANCE."
  (make-instance 'standard-class :direct-superclasses
                 (list (find-class 'bot-user)) :name
                 `(,(nickname user-instance) ,(irc:hostname user-instance))))

(defun change-user-class-to-bot-user (user-instance)
  "Change class of USER-INSTANCE to anon class."
  (change-class user-instance (make-anon-bot-user-class user-instance)))

(defmethod change-bot-user-class ((connection connection) (user irc:user))
  (change-class user (c2cl:ensure-class
                      (format-symbol (ensure-network-package
                                      (irc:server-name connection))
                                     "~A" (irc:hostname user))
                      :direct-superclasses (list (find-class 'bot-user)))))


(defclass target (abstract-target) ())

(defclass bot-user (irc:user target abstract-user) ()
  (:documentation "This does not bother with normalization.

All things made by `make-anon-bot-user-class' superclass this."))

(defclass bot-channel (irc:channel target abstract-address) ())

(defclass irc-user (abstract-target)
  ((user :initarg :user :type bot-user :reader name)
   (address :initarg :address :type target :reader address)))

(defclass irc-message-content (abstract-text-message-content)
  ())

(defmethod commandp ((object irc-message-content))
  (not (string= (full-message object) (message object))))

(defmethod nickname ((irc connection))
  "Return CONNECTION's nickname."
  (nickname (irc:user irc)))

(defmethod username ((irc connection))
  "Return CONNECTION's username."
  (username (irc:user irc)))

(defmethod realname ((irc connection))
  "Return CONNECTION's realname."
  (realname (irc:user irc)))

(defmethod irc:privmsg ((irc irc:connection) target message)
  "Turn MESSAGE into a string before sending through CONNECTION to TARGET."
  (irc:privmsg irc target (delete #\Newline (princ-to-string message))))

(defgeneric message-text (object))
(defmethod message-text ((msg irc:irc-message))
  "Return the string containing the irc-message."
  (first (last (irc:arguments msg))))

(defmethod find-channel-user ((connection connection)
                              (channel irc:channel) (nickname string))
  "Find NICKNAME in CHANNEL on CONNECTION."
  (let ((nickname (irc:normalize-nickname connection nickname)))
    (or (gethash nickname (irc:users channel))
        (when (string= nickname (nickname (irc:user connection)))
          (irc:user connection)))))


(defmethod target ((message irc:irc-privmsg-message))
  "String with message target."
  (car (irc:arguments message)))

(defmethod reply-target ((message irc:irc-privmsg-message))
  "Find location to reply to MESSAGE."
  (if (string= (nickname (irc:user (irc:connection message)))
               (target message))
      (irc:find-user (irc:connection message) (irc:source message))
      (target message)))

(defmethod irc-handle-privmsg ((message irc:irc-privmsg-message) &aux
                               (irc (irc:connection message))
                               (source (irc:source message))
                               (to (reply-target message))
                               (cmd (message-text message)))
  (handler-case
      (when (and (> (length cmd) 0)
                 (find (comchar irc) cmd :end 1))
        (route-command
         irc
         (make-instance 'irc-user
                        :address (typecase to
                                   (irc:channel (ensure-irc-bot-channel to))
                                   (string (ensure-irc-bot-channel
                                            (irc:find-channel irc to)))
                                   (irc:user (ensure-irc-bot-user to)))
                        :user (ensure-irc-bot-user
                               (ensure-user-host (irc:find-user irc source)
                                                 (irc:host message))))
         (make-instance 'irc-message-content :message cmd
                        :bot-connection irc)
         (typecase to
           (irc:channel (ensure-irc-bot-channel to))
           (string (ensure-irc-bot-channel
                    (irc:find-channel irc to)))
           (irc:user (ensure-irc-bot-user to)))
         irc))
    (error (condition) (describe condition))))

(defmethod ensure-user-host ((user irc:user) (host string))
  (when (string= (irc:hostname user) "")
    (setf (irc:hostname user) host))
  user)

;;;{{{ IRC user superclasses
(defgeneric add-irc-user-superclass (connection user superclass)
  (:documentation "Add SUPERCLASS to USER on CONNECTION."))
(defgeneric delete-irc-user-superclass (connection user superclass)
  (:documentation "Remove SUPERCLASS from USER on CONNECTION."))


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
  "Find NICKNAME in CONNECTION and pass on."
  (add-irc-user-superclass irc
                           (irc:find-user irc nickname)
                           superclass))
(defmethod delete-irc-user-superclass ((irc connection) (user bot-user)
                                       (superclass symbol))
  (declare (ignore irc))
  (delete-superclass-of user superclass))
(defmethod delete-irc-user-superclass ((irc connection) (nickname string)
                                       (superclass symbol))
  "Find NICKNAME in CONNECTION and pass on."
  (delete-irc-user-superclass irc (irc:find-user irc nickname) superclass))
;;;}}}

(defgeneric remove-comchar (comchar message)
  (:documentation
   "Remove COMCHAR from MESSAGE. ~

    Methods should return `nil' if COMCHAR is not the first `character'~
    in MESSAGE."))

(defmethod remove-comchar ((comchar character) (message string))
  (declare (type valid-comchar comchar))
  (check-type comchar valid-comchar)    ;Actually care its the right type.
  (when (char= comchar (aref message 0))
    (subseq message 1)))

(defmethod remove-comchar ((comchar string) (message string))
  (declare (type valid-comchar-string comchar))
  (remove-comchar (character comchar) message))

(defmethod remove-comchar ((comchar comchar) message)
  "Remove leading COMCHAR from MESSAGE."
  (remove-comchar (comchar comchar) message))

(defun ensure-irc-bot-channel (channel-object)
  (declare (type irc:channel))
  (if (typep channel-object 'bot-channel)
      channel-object
      (change-class channel-object 'bot-channel)))

(defun ensure-irc-bot-user (user-object)
  (declare (type irc:user))
  (if (typep user-object 'bot-user)
      user-object
      (change-class user-object 'bot-user)))

(defmethod initialize-instance :after ((instance irc-message-content)
                                       &rest initargs &key bot-connection)
  (declare (ignore initargs)
           (type bot-connection bot-connection))
  (setf (slot-value instance 'message)
        (or (remove-comchar (comchar bot-connection)
                            (slot-value instance 'full-message))
            (slot-value instance 'full-message)))
  (setf (slot-value instance 'remaining-message)
        (slot-value instance 'message)))


(defgeneric connectedp (connection-object)
  (:documentation "Is CONNECTION-OBJECT currently connected?"))
(defmethod connectedp ((irc irc:connection))
  (and (slot-boundp irc 'irc:output-stream)
       (open-stream-p (irc:output-stream irc))))

(defgeneric handle-nisp-command (tree source from address identity
                                      action content)
  (:generic-function-class nisp-command-network-tree-generic-function)
  (:method-class handle-nisp-command-method))

(defmacro define-simple-command (name &body body)
  `(defmethod handle-nisp-command
       ((tree (eql #-sbcl(network-tree::intern-network-tree-node
                     ,(substitute #\Space #\- (symbol-name name)))
                   #+sbcl ,(substitute #\Space #\- (symbol-name name))))
        (source abstract-data-source)
        (user abstract-user)
        (address abstract-target)
        (identity abstract-identity)
        (action abstract-action)
        (content abstract-text-message-content))
     ,@body))

(defmethod send (action
                 (sink bot-connection)
                 (to target) (content string))
  (declare (ignore action))
  "Default action for irc is to privmsg"
  (privmsg sink to content))
(defmethod send (action
                 (sink bot-connection)
                 (to target) (content cons))
  (declare (ignore action))
  "Default action for irc is to privmsg"
  (privmsg sink to (remove #\Newline
                           (format nil "~{~A~^ ~}" content))))

(defmethod privmsg ((sink bot-connection) (to target)
                    (content string))
  (irc:privmsg sink to content))


(define-simple-command emacs
  (network-tree::next-node))
(define-simple-command source
  (reply "I'm written in common lisp by nixeagle. You can find my source at <http://github.com/nixeagle/nisp/tree/master/irc-bot/>"))
(define-simple-command say
  (reply (remaining-parameters)))


(defgeneric route-command (source from content to sink))
(defmethod route-command :around (source from content to sink)
  "Time how long calls and what parems were used and put this timing data
  in a list."
  (let ((start-time (get-internal-real-time)))
    (flet ((push-new-time (result)
             (let ((end-time (get-internal-real-time)))
               (push (list source from content to sink result
                           (/ (- end-time start-time)
                              internal-time-units-per-second))
                     *route-call-times*))
             result))
      (handler-bind ((error (lambda (condition)
                              (print condition)
                              (push-new-time condition)
                              (error condition))))
        (push-new-time (call-next-method))))))
(defmethod route-command  ((source abstract-data-source)
                           (from abstract-from)
                           (content abstract-message-content)
                           (to abstract-target)
                           (sink abstract-data-sink))
  (declare (ignore sink))
  (when (commandp content)
    (handle-nisp-command (message content) source (name from)
                         to (make-instance 'abstract-identity)
                         (make-instance 'abstract-action) content)))

#+ ()
(defmethod handle-nisp-command ((tree (eql "say"))
                                (source abstract-data-source)
                                (user abstract-user)
                                (address abstract-address)
                                (identity abstract-identity)
                                (action abstract-action)
                                (content abstract-message-content))
  (privmsg source address (network-tree::remaining-parameters)))

;;; END
