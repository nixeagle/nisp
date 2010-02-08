(in-package :nisp.i)

(defpackage #:nisp.i.command-argument-symbols
  (:use))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *command-argument-symbols-package* :nisp.i.command-argument-symbols
    "Package to intern symbols for command argument parsing.")
  (make-empty-package *command-argument-symbols-package* :supersede))

(defun split-command-string (command-string)
  "Split COMMAND-STRING into appropriate parts."
  (declare (type string command-string))
  (split-sequence #\Space command-string :remove-empty-subseqs t))

(defun %generate-interned-argument-symbols (count &optional (format-spec "ARG~A"))
  "Intern as if by FORMAT-SPEC a list up to COUNT symbols."
  (declare (type non-negative-fixnum count))
  (iter (for n :from 1 :to count)
        (collect (format-symbol t format-spec n))))
(defun %format-command-handler-symbol (count &optional
                                       (format-spec "COMMAND-HANDLER-~A-ARGS"))
  (format-symbol t format-spec count))
(defparameter +command-routing-functions+
  (flet ((define-handler (count)
           "Define a `command-handler-COUNT-args' generic function."
           (ensure-generic-function
            (%format-command-handler-symbol count)
            :lambda-list `(connection sender to cmd
                                      ,@(%generate-interned-argument-symbols count)
                                      &optional remaining)
            :documentation (format nil "Handle commands with ~A arguments" count))))
    (let ((maximum-routing-arguments 15))
      (make-array (1+ maximum-routing-arguments)
                  :initial-contents
                  (iter (for n from 0 to maximum-routing-arguments)
                        (collect (define-handler n)))
                  :element-type 'STANDARD-GENERIC-FUNCTION)))
  "Array of generic functions for routing commands.")

(defun %find-command-handler-generic (count &optional
                                      (array +command-routing-functions+))
  "Find generic function with COUNT command parameters in VECTOR"
  (declare (type non-negative-fixnum count)
           (type simple-array array))
  (aref array count))


(defgeneric find-command-handler-generic (&rest arguments)
  (:documentation "Look up command handler by ARGUMENTS."))
(defmethod find-command-handler-generic (&rest more-arguments)
  (%find-command-handler-generic
   (typecase (car more-arguments)
     (string (length more-arguments))
     (integer (car more-arguments))
     (cons (length (car more-arguments)))
     (t 0))))

(defun %format-command-method-symbol (name)
  (declare (type symbol name))
  (let ((params (split-sequence #\- (symbol-name name))))
    (values (length params) (mapcar (lambda (x)
                                      (ensure-symbol
                                       x
                                       *command-argument-symbols-package*))
                                    params))))

(defmethod ensure-string ((arg string))
  "Return ARG as it is already a string."
  arg)
(defmethod ensure-string ((arg symbol))
  (symbol-name arg))


(defmethod filter-to-remaining-arguments ((full-command string)
                                          (current-args list))
  "Convert CURRENT-ARGS to a string and call again."
  (filter-to-remaining-arguments full-command
                                 (format nil "~{~A~^ ~}" current-args)))
(defmethod filter-to-remaining-arguments ((full-command string)
                                          (current-args string))
  (subseq full-command (+ (search (string-upcase current-args)
                                  (string-upcase full-command))
                          (length current-args))))
(defmethod filter-to-remaining-arguments :around ((full-command string)
                                                  (current-args string))
  "Remove leading space if there is one."
  (let ((result (call-next-method)))
    (if (and (> (ARRAY-DIMENSION result 0) 0)
             (eq (aref result 0) #\Space))
        (subseq result 1)
        result)))

(defmethod %route-command (connection sender to cmd current-args remaining-args)
  "Route based on CURRENT-ARGS and leaving the rest in REMAINING-ARGS."
  (handler-case
      (apply
       (find-command-handler-generic current-args)
       connection sender to cmd
       (append current-args
               (ensure-list
                (filter-to-remaining-arguments cmd current-args))))
    (error (condition) (describe condition)
           :nisp-no-command-found)))

(defmethod route-command (connection sender to cmd
                          &rest split-cmd)
  (%route-command connection sender to cmd
                  (list (ensure-symbol (string-upcase (caar split-cmd))
                                       *command-argument-symbols-package*))
                  (cdar split-cmd)))

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

(defclass bot-user (irc:user) ()
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

(defmethod shared-initialize :after ((bot bot-connection) slot-names
                                     &key nickname username realname)
  (when (and nickname username realname)
    (setf (slot-value bot 'irc:user)
          (make-instance 'bot-user
                         :nickname nickname 
                         :username username
                         :realname realname))))

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

(defmethod connect :after ((irc 8b-i-bot-connection) &key)
  "Connect I bot to #offtopic and #bots on eighthbit.net"
  (irc:join irc "#offtopic")
  (irc:join irc "#bots")
  (irc:add-hook irc 'irc:irc-privmsg-message 'irc-handle-privmsg))

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
(defclass target (irc:user irc:channel) ())
(defgeneric handle-command (connection sender to cmd))

(defmethod handle-command ((irc connection) sender (to string) cmd)
  "Look up which channel TO is talking about and pass it along."
  (handle-command irc sender (irc:find-channel irc to) cmd))

(defmethod handle-command ((irc connection) sender (to irc:channel) cmd)
  (handle-command irc
                  (ensure-user-host (irc:find-user irc (irc:source sender))
                                    (irc:host sender))
                  to cmd))

(defmethod handle-command ((irc connection) sender (to irc:user) cmd)
  (handle-command irc
                  (ensure-user-host (irc:find-user irc (irc:source sender))
                                    (irc:host sender))
                  to cmd))

(defmethod remove-comchar ((comchar comchar) (message string))
  "Remove leading COMCHAR from MESSAGE."
  (when (find (comchar comchar) message :end 1)
    (subseq message 1)))

(defmethod handle-command ((irc bot-connection) (sender irc:user)
                           to cmd)
  (handler-case 
      (when (and (> (length cmd) 0)
                 (find (comchar irc) cmd :end 1))
        (route-command irc sender to (remove-comchar irc cmd)
                                     (split-command-string
                                      (remove-comchar irc cmd))))
    (error (condition) (irc:privmsg irc to condition))))

(defun join-sequence (sequence &optional (seperator " "))
  (format nil (concatenate 'string "~{~A~^" seperator "~}") sequence))

(test (join-sequence :suite nil)
  (with-fbound (join-sequence)
    ('("a" "b")) "a b"))

(define-command-node test (8b-i-bot-connection irc:user irc:channel string
                                               params))
(define-command test-run (8b-i-bot-connection irc:user irc:channel string
                                               params)
  (irc:privmsg 8b-i-bot-connection irc:channel
               (iterate (for x in (run
                                   (ensure-symbol
                                    (string-upcase
                                     (car (split-command-string params))))))
                        (counting (typep x 'eos::test-passed) :into passed)
                        (counting (not (typep x 'eos::test-passed)) :into failed)
                        (finally
                         (return
                           (format nil
                                   "Total tests: ~A Passed: ~A Failed: ~A"
                                   (+ passed failed)
                                   passed failed))))))

(define-command-node github (connection irc:user (to t) string params))
(define-command-node github-show (connection irc:user (to t)
                                             string params))
(define-command github-show-followers (connection irc:user (to irc:channel) 
                                                  string github-user)
  (irc:privmsg connection to 
               (join-sequence (clithub:show-followers github-user))))
(define-command github-show-following (connection irc:user irc:channel
                                                  string github-user)
  (irc:privmsg connection irc:channel
               (join-sequence (clithub:show-following github-user))))
(define-command github-show-following (connection irc:user (to irc:user) 
                                                  string github-user)
  "Handling a private message is possible."
  (irc:privmsg connection to github-user))


;;; Types
