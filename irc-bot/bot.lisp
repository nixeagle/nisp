(in-package :nisp.i)

;;;{{{ Random helpers:
(defun remove-newlines (string)
  "Remove newlines from STRING."
  (declare (type string string))
  (remove #\Newline string))

(defun join-sequence (sequence &optional (seperator " "))
  (format nil (concatenate 'string "~{~A~^" seperator "~}") sequence))

(test (join-sequence :suite nil)
  (with-fbound (join-sequence)
    ('("a" "b")) "a b"))
;;;}}}

(defclass connection (irc:connection) ())

;;;{{{ define-command:
(defmacro %define-command (name (irc sender to msg remaining) &body body)
  (multiple-value-bind (count params)
      (%format-command-method-symbol name)
    `(defmethod ,(%format-command-handler-symbol count)
         (,(if (consp irc) irc `(,irc ,irc))
          ,(if (consp sender) sender `(,sender ,sender))
          ,(if (consp to) to `(,to ,to))
          ,(if (consp msg) msg `(,msg ,msg))
          ,@(iter (for x in (%generate-interned-argument-symbols count))
                  (for y in params)
                  (collect `(,x (eql ',y))))
          &optional ,remaining)
       ,@body)))

(defmacro define-command (name (irc sender to msg remaining) &body body)
  `(%define-command ,name (,irc ,sender ,to ,msg ,remaining)
       (declare (ignore ,@(%generate-interned-argument-symbols
                           (%format-command-method-symbol name))))
       ,@body))

(defmacro define-command-node (name (irc sender to msg remaining) &body body)
  (let ((count (%format-command-method-symbol name)))
    `(%define-command ,name (,irc ,sender ,to ,msg ,remaining)
       ,@body
       (,(%format-command-handler-symbol (1+ count))
         ,(if (consp irc) (car irc) irc)
         ,(if (consp sender) (car sender) sender)
         ,(if (consp to) (car to) to)
         ,(if (consp msg) (car msg) msg)
         ,@(%generate-interned-argument-symbols count)
         (ensure-symbol
          (string-upcase (car (split-command-string ,remaining)))
          *command-argument-symbols-package*)
         (filter-to-remaining-arguments
          ,remaining
          (string-upcase (car (split-command-string ,remaining))))))))

;;;}}}

;;; not bot related directly
;;;{{{ superclasses:
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

(defmethod change-bot-user-class ((connection connection) (user irc:user))
  (change-class user (c2cl:ensure-class
                      (format-symbol (ensure-network-package
                                      (irc:server-name connection))
                                     "~A" (irc:hostname user))
                      :direct-superclasses (list (find-class 'bot-user)))))
;;;}}}

(defvar *irc-bot-instances* nil
  "Global list of bot instances.")
(defclass target () ())

(defclass bot-user (irc:user target abstract-user) ()
  (:documentation "This does not bother with normalization.

All things made by `make-anon-bot-user-class' superclass this."))

(defclass bot-channel (irc:channel target abstract-address) ())

(defclass irc-user (abstract-target)
  ((user :initarg :user :type bot-user :reader name)
   (address :initarg :address :type bot-channel :reader address)))

(defclass irc-message-content (abstract-message-content)
  ((full-message :initarg :message :reader full-message :type string)
   (message :reader message :type string)
   (remaining-message :reader remaining-message :type string)))
(defmethod commandp ((object irc-message-content))
  (not (string= (full-message object) (message object))))

;;;{{{ connection classes
(defclass bot-connection (connection comchar
                                     abstract-data-source
                                     abstract-data-sink) ())
(defclass connect-with-background-handler-mixin () ()
  (:documentation "Superclass this to connect directly to background on
methods that support this."))
(defclass lo-i-bot-connection (bot-connection
                               connect-with-background-handler-mixin)
  ()
  (:default-initargs :username "lisp" :nickname "i"
                     :realname "bot" :server-port 9999
                     :server-name "127.0.0.1"
                     :comchar ",")
  (:documentation "Specifically for testing disconnect behavior."))

(defclass 8b-i-bot-connection (bot-connection
                               connect-with-background-handler-mixin)
  ()
  (:default-initargs :username "lisp" :nickname "i"
                     :realname "bot" :server-port 6667
                     :server-name "irc.eighthbit.net"
                     :comchar ",")
  (:documentation "blah"))

(defclass 8b-nisp-bot-connection (bot-connection
                               connect-with-background-handler-mixin)
  ()
  (:default-initargs :username "lisp" :nickname "nisp"
                     :realname "bot" :server-port 6667
                     :server-name "irc.eighthbit.net"
                     :comchar "|")
  (:documentation "blah"))

(defclass slack-nisp-bot-connection (bot-connection
                                     connect-with-background-handler-mixin)
  ()
  (:default-initargs :username "lisp" :nickname "nisp"
                     :realname "Nixeagle's lisp experiments bot."
                     :server-port 6667
                     :server-name "74.208.99.237"
                     :comchar ",")
  (:documentation "Bot connection for slack's network."))

(defclass sonic-nisp-bot-connection (bot-connection
                                     connect-with-background-handler-mixin)
  ()
  (:default-initargs :username "lisp" :nickname "nisp"
                     :realname "Nixeagle's lisp experiments bot."
                     :server-port 6667
                     :server-name "irc.57o9.net"
                     :comchar ",")
  (:documentation "sonicrules1234's irc network."))

(defclass flare-nisp-bot-connection (bot-connection
                                     connect-with-background-handler-mixin)
  ()
  (:default-initargs :username "lisp" :nickname "nisp"
                     :realname "Nixeagle's lisp experiments bot."
                     :server-port 6667
                     :server-name "flare183.net"
                     :comchar ",")
  (:documentation "Bot connection for flare183's server."))

(defmethod shared-initialize :after ((bot bot-connection) slot-names
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

(defparameter @thread@ nil)
(defmethod connect :after ((irc connect-with-background-handler-mixin) &key)
  "Start IRC's command loop in a different thread."
  (setq @thread@
        (bordeaux-threads:make-thread
         (lambda ()
           (nisp-start-read-loop irc))
         :name (compute-connection-id irc))))

(defmethod connect :after ((irc 8b-i-bot-connection) &key)
  "Connect I bot to #offtopic and #bots on eighthbit.net"
  (irc:join irc "#offtopic")
  (irc:join irc "#bots")
  (irc:join irc "#nixeagle")
  (irc:add-hook irc 'irc:irc-privmsg-message 'irc-handle-privmsg))

(defmethod connect :after ((irc 8b-nisp-bot-connection) &key)
  "Connect I bot to #offtopic and #bots on eighthbit.net"
  (irc:join irc "#offtopic")
  (irc:join irc "#bots")
  (irc:join irc "#nixeagle")
  (irc:add-hook irc 'irc:irc-privmsg-message 'irc-handle-privmsg))

(defmethod connect :after ((irc slack-nisp-bot-connection) &key)
  (irc:add-hook irc 'irc:irc-privmsg-message 'irc-handle-privmsg)
  (sleep 1)
  (irc:join irc "#bots"))

(defmethod connect :after ((irc sonic-nisp-bot-connection) &key)
  (irc:add-hook irc 'irc:irc-privmsg-message 'irc-handle-privmsg)
  (irc:join irc "#services")
  (irc:join irc "#sonicircd"))

(defmethod connect :after ((irc flare-nisp-bot-connection) &key)
  (irc:add-hook irc 'irc:irc-privmsg-message 'irc-handle-privmsg)
  (irc:join irc "#help"))

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
(defmethod irc-handle-privmsg ((message irc:irc-privmsg-message))
  "i bot test."
  (setq *it* message)
  (handler-case
      (handle-command (irc:connection message)
                      message
                      (reply-target message)
                      (message-text message))
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

(defgeneric handle-command (connection sender to cmd))

(defmethod handle-command ((irc connection) sender (to string) cmd)
  "Look up which channel TO is talking about and pass it along."
  (handle-command irc sender (irc:find-channel irc to) cmd))

(defmethod handle-command ((irc connection) sender (to irc:channel) cmd)
  (handle-command irc
                  (ensure-user-host (irc:find-user irc (irc:source sender))
                                    (irc:host sender))
                  (ensure-irc-bot-channel to) cmd))

(defmethod handle-command ((irc connection) sender (to irc:user) cmd)
  (handle-command irc
                  (ensure-user-host (irc:find-user irc (irc:source sender))
                                    (irc:host sender))
                  (ensure-irc-bot-user to)
                  cmd))
(defmethod handle-command ((irc bot-connection) (sender irc:user)
                           to cmd)
  (handler-case
      (when (and (> (length cmd) 0)
                 (find (comchar irc) cmd :end 1))
        (route irc (make-instance 'irc-user :address to
                                  :user (ensure-irc-bot-user sender))
               (make-instance 'irc-message-content :message cmd
                              :bot-connection irc) t t))
    (error (condition) (irc:privmsg irc to condition))))

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
;;;{{{ Define commands

;;;{{{ Github commands

(define-command-node github (connection irc:user (to t) string params))
(define-command-node github-show (connection irc:user (to t)
                                             string params))
(define-command github-show-followers (connection irc:user target
                                                  string github-user)
  (irc:privmsg connection target
               (join-sequence (cl-github:show-followers github-user))))
(define-command github-show-following (connection irc:user target
                                                  string github-user)
  "Private message TARGET with list of people GITHUB-USER follows."
  (irc:privmsg connection target
               (join-sequence (cl-github:show-following github-user))))

;;;}}}

;;;{{{ Nisp introspection commands

(define-command-node nisp (connection irc:user target string params))
(define-command-node nisp-introspect (connection irc:user target string params))
(define-command-node nisp-introspect-irc (connection irc:user target
                                                     string params))
(define-command nisp-introspect-irc-user (connection irc:user target
                                                       string nickname)
  (irc:privmsg connection target
               (nisp.mop::class-slot-name-value-alist
                (irc:find-user connection nickname))))

;;;}}}

;;;{{{ Unit test runner
(define-command-node test (8b-i-bot-connection irc:user irc:channel string
                                               params))

(define-command test-run (8b-i-bot-connection irc:user irc:channel string
                                              params)
  (irc:privmsg 8b-i-bot-connection irc:channel
               (generate-short-test-summary (if (string= params "all")
                                                :nisp-eos-root
                                                params))))
;;;}}}

;;;{{{ Alpha commands <highly experimental>
(define-command-node alpha (connection irc:user target string params))

(define-command alpha-apropos (connection irc:user target string params)
  "Look up applicatable methods."
  (irc:privmsg connection target (test-lookup params)))

(define-command alpha-tnt (connection irc:user target string params)
    "Testing command for blowing stuff up, hence \"tnt\"."
    (irc:privmsg connection target
                 (prin1-to-string
                  (nisp.mop-store::generate-slot-specifier
                   (car (nisp.mop-simple:class-slots
                         (find-symbol (string-upcase params))))))))

(define-command alpha-nikurl (connection irc:user target string params)
  "Shorturl something."
  (irc:privmsg connection target
               (car (cl-ppcre:all-matches-as-strings
                     "http\\S+"
                     (drakma:http-request
                      (concatenate 'string
                                   "http://nik.im/api_create.php?url="
                                   (remove #\Space params)))))))
;;;}}} Alpha commands
;;;}}} Define commands

(defgeneric connectedp (connection-object)
  (:documentation "Is CONNECTION-OBJECT currently connected?"))
(defmethod connectedp ((irc irc:connection))
  (and (slot-boundp irc 'irc:output-stream)
       (open-stream-p (irc:output-stream irc))))

;;;{{{ Initialize bots hacks <irc only right now>
(defvar *sonic*)
(defvar *slack*)
(defvar *bot*)
(defvar *devel-bot* nil "bot hosted on my laptop as opposed to the vps.")
(defvar *flare*)
(defvar @lo@)
(defun %initialize-bots ()
  "Start up all the bots."
  (macrolet ((define-bot (name class)
               `(defparameter ,name (make-instance ',class))))
    (define-bot *sonic* sonic-nisp-bot-connection)
    (define-bot *slack* slack-nisp-bot-connection)
    (define-bot *bot* 8b-i-bot-connection)
    (define-bot *devel-bot* 8b-nisp-bot-connection)
    (define-bot *flare* flare-nisp-bot-connection))
  (setq *format-and-send-to-irc-function* (curry #'irc:privmsg *bot* "#bots")))

(defparameter %bot-list% '(*sonic* *slack* *bot* *flare* *devel-bot* @lo@)
  "Hackish list of bots.")

;;;}}}


;;; past hacks now into attempting to create a generic routing protocol

(defun unbind-symbols (&rest symbols)
  (mapcar (conjoin #'symbolp #'boundp #'makunbound) symbols)
  (mapcar (conjoin #'symbolp #'fboundp #'fmakunbound) symbols))

(defclass handle-nisp-command-method (network-tree::network-tree-method)
  ())


(defgeneric send (action to sink content))
(defgeneric privmsg (sink to content))
(defmethod privmsg ((sink bot-connection) (to target)
                    (content string))
  (irc:privmsg sink to content))
(defgeneric handle-nisp-command (tree source from address identity
                                      action content)
  (:generic-function-class network-tree::network-tree-generic-function)
  (:method-class handle-nisp-command-method))
(defmethod handle-nisp-command ((tree (eql "say"))
                                (source abstract-data-source)
                                (user abstract-user)
                                (address abstract-address)
                                (identity abstract-identity)
                                (action abstract-action)
                                (content abstract-message-content))
  (privmsg source address (network-tree::remaining-parameters)))
(defgeneric route (source from content to sink))
(defmethod route  ((source abstract-data-source)
                   (from abstract-from)
                   (content abstract-message-content)
                   to sink)
  (declare (ignore to sink))
  (when (commandp content)
    (handle-nisp-command (message content) source (name from)
                         (address from) (make-instance 'abstract-identity)
                         (make-instance 'abstract-action) content)))


;;; END
