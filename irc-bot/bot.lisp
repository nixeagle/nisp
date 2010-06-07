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


(defmethod message-text ((msg irc:irc-message))
  "Return the string containing the irc-message."
  (first (last (irc:arguments msg))))

(defmethod find-channel-user ((connection connection)
                              (channel irc:channel) (nickname string))
  "Find NICKNAME in CHANNEL on CONNECTION."
  (or (irc:normalize-nickname connection nickname)
      (gethash (irc:normalize-nickname connection nickname) (irc:users channel))
      (when (string= (irc:normalize-nickname connection nickname) (nickname (irc:user connection)))
        (irc:user connection))))


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
      (when (and (> (length cmd) 0))
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

(defmethod remove-name ((irc bot-connection) (message string))
  "Remove bot's name if its addressed."
  (let ((nickname-length (length (nickname irc))))
    (and (search message (nickname irc) :end1 nickname-length)
         (member (char message nickname-length)
                 '(#\: #\,) :test #'char=)
         (subseq message (+ 2 nickname-length)))))

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
            (remove-name bot-connection (slot-value instance 'full-message))
            (slot-value instance 'full-message)))
  (setf (slot-value instance 'remaining-message)
        (slot-value instance 'message)))

(defvar *command-args*)
(defmethod route-command  ((source abstract-data-source)
                           (from abstract-from)
                           (content abstract-message-content)
                           (to abstract-target)
                           (sink abstract-data-sink))
  (declare (ignore sink))
  (if (commandp content)
      (progn
        (setq *command-args* (list (message content) source (name from)
                                   to (make-instance 'abstract-identity)
                                   (make-instance 'abstract-action) content))
        (handle-command (message content) source (name from)
                     to (make-instance 'abstract-identity)
                     (make-instance 'abstract-action) content))
      (let ((it (parse-link (message content))))
        (when it
          (handle-command (format nil "link ~A ~A" (aref it 0) (aref it 1))
                     source (name from)
                     to (make-instance 'abstract-identity)
                     (make-instance 'abstract-action) content)))))

(defmethod connectedp ((irc irc:connection))
  (and (slot-boundp irc 'irc:output-stream)
       (open-stream-p (irc:output-stream irc))))

(defmethod send (action
                 (sink bot-connection)
                 (to target) (content string) &rest additional-args)
  (declare (ignore action))
  "Default action for irc is to privmsg"
  (privmsg sink to (remove #\Newline
                           (if additional-args
                               (apply #'format nil content additional-args)
                               (format nil "~A" content)))))
(defmethod send (action
                 (sink bot-connection)
                 (to target) (content cons) &rest additional-args)
  (declare (ignore action additional-args))
  "Default action for irc is to privmsg"
  (privmsg sink to (remove #\Newline
                          #+ () (format nil "~{~A~^ ~}" content)
                          (format nil "~A" content))))

(defmethod privmsg ((sink bot-connection) (to target)
                    (content string))
  (irc:privmsg sink to content))

(defun multiarray-string-p (matches-array)
  "Match a wikilink style thing that is really the inside of a multi
dimensional array."
  ;; Nasty regex here. We are matching ] [ ] or ] , [ and not allowing
  ;; link completion on those patterns. The spaces in those examples can
  ;; be any number of characters.
  (and (not (cl-ppcre:scan "\\\][^\\\[]*\\\[.*\\\]|\\\][^\\\[]*,[^\\\[]*\\\["
                             (aref matches-array 1)))
         matches-array))

(defun parse-link (line)
  (multiple-value-bind (whole-match matches-array)
      (cl-ppcre:scan-to-strings "\\\[\\\[(?:([^:]+):)?(.*?)\\\]\\\]" line)
    (declare (ignore whole-match))
    (multiarray-string-p matches-array)))


(defun string-integer-p (string &optional (radix 10))
  (= (length string) (nth-value 1 (parse-integer string :junk-allowed t
                                                 :radix radix))))



(define-simple-command lag
  (reply "~A seconds." (float (/ (random 10000) (random 10000)))))

(define-simple-command emacs
  (network-tree::next-node))
(define-simple-command source
  (reply "I'm written in common lisp by nixeagle. You can find my source at <http://github.com/nixeagle/nisp/tree/master/irc-bot/>."))
(define-simple-command emacs-live
  (reply "Nixeagle's live emacs buffer is at http://i.nixeagle.net:1337/t or http://i.nixeagle.net:8080/t . For best results please view in firefox or chrome. Opera won't even show the text (source unknown). IE gets some silly runtime error and fixing it is not a priority."))
(define-simple-command say (reply (remaining-parameters)))

(define-simple-command shorturl
  (reply (shorturl-is.gd (remaining-parameters))))

(defvar *nist-compsci-dictionary* (make-hash-table :test #'equalp))


(defun string-capitalize-only (string)
  (cl-ppcre:regex-replace-all "\\\b(.)"
                              string
                              (lambda (target-string start end match-start match-end reg-starts reg-ends)
                                (declare (ignore start end reg-ends reg-starts))
                                (string-upcase (subseq target-string match-start match-end)))))




(define-simple-command help
  (reply "Help? what help"))

(define-simple-command about
  (network-tree::next-node))
(define-simple-command about-nass
  (reply "Nixeagle's attempt at an assembler. Source is at <http://github.com/nixeagle/nass>"))
(define-simple-command about-nisp
  (reply "Nisp is this bot, but more generally refers to a pile of lisp code at <http://github.com/nixeagle/nisp>. Check out some of the many sub directories in this repository for more information. For example this bot's code is in the irc-bot/ directory."))
(define-simple-command about-binary
  (network-tree::next-node))
(define-simple-command about-binary-data
  (reply "binary-data is at <http://github.com/nixeagle/binary-data>. By using the common lisp MOP, all kinds of binary formats can be described. Currently implemented: flat little endian structures. TODO: flat big endian structures (PPC, MIPS...), nested binary structures (ELF, communication protocols, id3 tags...)."))
(define-simple-command about-commits
  (reply "Commits for this bot are at: http://github.com/nixeagle/nisp/commits/master"))

(define-simple-command regex
  (let ((match-array (nth-value 1 (cl-ppcre:scan-to-strings "/(.+?)/ (.+)"  (remaining-parameters)))))
  (reply
   (multiple-value-bind (start end reg-start reg-end)
       (cl-ppcre:scan
        (aref match-array 0)
        (aref match-array 1))
     (if start
         (format nil "match: \"~A\"~A" (subseq (aref match-array 1) start end)
                 (if (length= 0 reg-start)
                     ""
                     (format nil "; Groups: [~{~A~^, ~}]"
                             (mapcar (lambda (re-start re-end)
                                       (format nil "\"~A\" (~A ~A)"
                                               (subseq (aref match-array 1) re-start re-end)
                                               re-start re-end))
                                     (coerce reg-start 'list)
                                     (coerce reg-end 'list)))))
         "Does not match")))))


(define-simple-command ping (reply "pong"))

(define-simple-command about-parenphobia
  (reply "Parenphobia is the result of excessive attention paid to syntatic padding chars and not enough attention to meaning of the code. See http://img264.imageshack.us/img264/1397/lispnd7.png for how a lisper sees it."))

(define-simple-command about-dux
  (reply "Dux is a simple operating system started in 2008, with goals including being easily ported to new systems and being simple to understand. Source is at http://github.com/RockerMONO/dux and more information is at http://duckinator.net/dux"))

(define-simple-command about-smart
  (network-tree::next-node))
(define-simple-command about-smart-questions
  (reply "See http://www.catb.org/~esr/faqs/smart-questions.html for hints on asking a better question."))

(define-simple-command bits
  (when (string-integer-p (remaining-parameters) 16)
    (let ((value (parse-integer (remaining-parameters) :radix 16 :junk-allowed t)))
      (reply
       (if (zerop value)
           "0000"
           (apply #'concatenate 'string
                  (loop for i from (* 8 (floor (log value 256))) downto 0 by 8
                     collect (format nil "~8,'0,' ,8:B " (ldb (byte 8 i) value)))))))))

(define-simple-command hexs
  (when (string-integer-p (remaining-parameters) 2)
    (let ((value (parse-integer (remaining-parameters) :radix 2 :junk-allowed t)))
      (reply (format nil "~x" value)))))

(define-simple-command link-x86opcode
  (reply "http://ref.x86asm.net/geek.html#x~A" (remaining-parameters)))

;;; harder say stuff... not at all best way to do this
(defmethod handle-command
    ((tree (eql "privmsg")) (source connection) (user bot-user)
     (address bot-channel) (identity abstract-identity)
     (action abstract-action) (content abstract-text-message-content))
  (when (member (irc:hostname user)
                '("Bit/Cam" "Byte/nixeagle" "Byte/duckinator/Dux"
                  "Byte/CodeBlock")
                :test #'equalp)

    (apply #'irc:privmsg source (coerce (nth-value 1 (cl-ppcre:scan-to-strings
                                                "([^\S]+) (.+)" (remaining-parameters)))
                                        'list))))

;;; END
