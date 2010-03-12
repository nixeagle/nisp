(in-package :nisp.i)


(defun remove-newlines (string)
  "Remove newlines from STRING."
  (declare (type string string))
  (remove #\Newline string))

(defun join-sequence (sequence &optional (seperator " "))
  (format nil (concatenate 'string "~{~A~^" seperator "~}") sequence))

(test (join-sequence :suite nil)
  (with-fbound (join-sequence)
    ('("a" "b")) "a b"))

(defun shorturl-is.gd (string)
  (declare (type string string))
  (drakma:http-request "http://is.gd/api.php"
                       :parameters `(("longurl" . ,string))))

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

(defvar *irc-bot-instances* nil
  "Global list of bot instances.")
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

;;;{{{ connection classes
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
;;;}}}

(defclass bot-admin () ())

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

;;; Method written with heavy cribbing from cl-irc in command.lisp.

;;;{{{ connect methods:
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
  (irc:add-hook irc 'irc:irc-privmsg-message 'irc-handle-privmsg))


;;;}}}

(defmethod target ((message irc:irc-privmsg-message))
  "String with message target."
  (car (irc:arguments message)))

(defmethod reply-target ((message irc:irc-privmsg-message))
  "Find location to reply to MESSAGE."
  (if (string= (nickname (irc:user (irc:connection message)))
               (target message))
      (irc:find-user (irc:connection message) (irc:source message))
      (target message)))

(defvar *it*)
(defmethod irc-handle-privmsg ((message irc:irc-privmsg-message) &aux
                               (irc (irc:connection message))
                               (source (irc:source message))
                               (to (reply-target message))
                               (cmd (message-text message)))
  "i bot test."
  (setq *it* message)
  (handler-case
      (when (and (> (length cmd) 0)
                 (find (comchar irc) cmd :end 1))
        (route irc
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
                              :bot-connection irc) t t))
    (error (condition) (describe condition))))

(defclass sender ()
  ((host :accessor host :initarg :host)
   (source :accessor source :initarg :source)
   (user :accessor user :initarg :user)))

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

(defmethod generate-short-test-summary ((suite symbol))
  "Run SUITE's tests and print a short report."
  (iterate (for test in (run suite))
           (counting (typep test 'eos::test-passed) :into passed)
           (counting (not (typep test 'eos::test-passed)) :into failed)
           (finally (print failed) (return
                      (format nil "Total tests: ~A Passed: ~A Failed: ~A"
                              (+ passed failed) passed failed)))))
(defmethod generate-short-test-summary ((suite string))
  "Split, upcase, and convert SUITE to a symbol then pass it on."
  (generate-short-test-summary
   (ensure-symbol (string-upcase (car (split-command-string suite))))))



(defmethod eql-specializer->string ((object closer-mop:eql-specializer))
  (string-downcase (symbol-name (closer-mop:eql-specializer-object object))))

(defun test-lookup (specializer
                    &optional (package :nisp.i.command-argument-symbols))
  (mapcar (lambda (x)
            (mapcar #'eql-specializer->string
                    (remove-if-not (lambda (object)
                                     (typep object 'closer-mop:eql-specializer))
                                   x)))
          (mapcar #'closer-mop:method-specializers
                  (closer-mop:specializer-direct-methods
                   (closer-mop:intern-eql-specializer
                    (find-symbol (string-upcase specializer) package))))))

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



(defun unbind-symbols (&rest symbols)
  (mapcar (conjoin #'symbolp #'boundp #'makunbound) symbols)
  (mapcar (conjoin #'symbolp #'fboundp #'fmakunbound) symbols))



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
  (privmsg sink to (remove-newlines (join-sequence content))))

(defmethod privmsg ((sink bot-connection) (to target)
                    (content string))
  (irc:privmsg sink to content))
(defgeneric handle-nisp-command (tree source from address identity
                                      action content)
  (:generic-function-class nisp-command-network-tree-generic-function)
  (:method-class handle-nisp-command-method))
(define-simple-command emacs
  (network-tree::next-node))
(define-simple-command source
  (reply "I'm written in common lisp by nixeagle. You can find my source at <http://github.com/nixeagle/nisp/tree/master/irc-bot/>"))


(define-simple-command beta
  (network-tree::next-node))

(define-simple-command beta-shorturl
  (reply (shorturl-is.gd (remaining-parameters))))


(define-simple-command say
  (reply (remaining-parameters)))


(defgeneric route (source from content to sink))
(defmethod route :around (source from content to sink)
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
(defmethod route  ((source abstract-data-source)
                   (from abstract-from)
                   (content abstract-message-content)
                   to sink)
  (declare (ignore to sink))
  (when (commandp content)
    (handle-nisp-command (message content) source (name from)
                         (address from) (make-instance 'abstract-identity)
                         (make-instance 'abstract-action) content)))


(load (merge-pathnames "config.lisp" +root-directory+)
      :if-does-not-exist nil)


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
