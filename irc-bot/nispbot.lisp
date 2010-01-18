(in-package :cl-user)
(defpackage #:nisp.irc
  (:use :common-lisp :iterate :metabang-bind))
(in-package :nisp.irc)

(defmacro ct (form type)
  `(let ((x ,form))
     (check-type x ,type (format nil "~S: ~A" ',type (documentation ',type 'type)))))

;; Idea for (documentation symbol 'type) taken from slime.lisp
(defun type-specifier-p (symbol)
  "True if SYMBOL is a type."
  (or (documentation symbol 'type)
      #+:sbcl
      (multiple-value-bind (arglist exists)
          (sb-introspect:deftype-lambda-list symbol)
        (declare (ignore arglist))
        exists)))

(deftype type-specifier ()
  "Type specifier that can be used with TYPEP"
  '(satisfies type-specifier-p))

(deftype positive-fixnum ()
  "A fixnum"
  `(integer 1 ,most-positive-fixnum))

(defun length<= (sequence upto-count)
  "When SEQUENCE is longer then UPTO-COUNT return nil.

If SEQUENCE is shorter then UPTO-COUNT return its length."
  (declare (type sequence sequence)
           (type positive-fixnum upto-count))
  (loop for char across sequence
          counting char into count 
          when (> count upto-count) return nil
     finally (return count)))
#+ (or)
(progn
  (assert (length<= "aaaaa" 5))
  (assert (length<= "aaaa" 5)))

(deftype letter-character () 
  "Represents an uppercase or lowercase letter in ASCII."
  '(member #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n
    #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D
    #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
    #\U #\V #\W #\X #\Y #\Z))

(deftype digit-character ()
  "Number character from 0 to 9 in base 10."
  '(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(deftype hex-digit-character ()
  "Number character from 0 to F in base 16"
  '(or
    digit-character
    (member #\A #\B #\C #\D #\E #\F)))

(deftype special-character ()
  "Represents extra non-digit/letter characters."
  '(member #\| #\[ #\] #\` #\^ #\\ #\_ #\{ #\}))

(deftype nickname-start-character ()
  "Valid character at the start of an IRC nickname."
  ;; These are taken from advice from duckinator on eighthbit.net/offtopic
  ;; Also see rfc 2812 sec: 2.3.1
  '(or 
    letter-character
    special-character))

(deftype nickname-character ()
  "Valid character after the first character of an IRC nickname."
  ;; Taken from advice from duckinator on eighthbit.net/offtopic
  '(or nickname-start-character
    (member digit-character #\-)))

(defun nickname-string-p (string)
  ;; Declaring the type here causes some problems with sbcl's
  ;; type derivatives. The last thing I want is:
  ;; (satisifies nickname-string-p) to get called _3_ times.
  ;; 
  ;; (declare (type base-string string))
  (and (typep (char string 0) 'nickname-start-character)
       (every 
        (lambda (x)
          (declare (type character x))
          (typep x 'nickname-character))
        (subseq string 1))))

(deftype nickname-string ()
  `(and 
    (vector character)
    (satisfies nickname-string-p)))

;; The meaning of these is specified in rfc2811
(deftype channel-start-character ()
  "Valid starting prefix of a channel name.

On most IRC networks # indicates a normal channel."
  ;; rfc2821 sec 1.3
  '(member #\& #\# #\+ #\!))

(deftype channel-character ()
  "Valid character in the name portion of a channel name."
  ;; rfc2821 sec 1.3
  `(and character
        (not (member #\Nul #\Bel #\, #\Space #\:))))

(defun channel-string-p (string)
  (and (typep (char string 0) 'channel-start-character)
       (every 
        (lambda (x)
          (declare (type character x))
          (typep x 'channel-character))
        (subseq string 1))))

;; Don't forget that these are also case insensitive.
(deftype channel-string (&optional size)
  "Valid channel name.

Needs to be m/[#!+&][^ \x007,:]+/"
  `(and 
    (vector character ,size)
    (satisfies channel-string-p)))

(deftype valid-integer-command ()
  "IRC numerical commands must be in the range 000 to 999."
  ;; This type does not verify that command is 001, only that it is 1
  `(integer 0 999))

;; Need some work to figure out how to validate strings.
(deftype valid-command ()
  "Commands must be an integer or a string.

Note that this type is incompletely defined."
  `(or valid-integer-command))

(deftype maximum-message-length ()
  "Longest SIZE an IRC message may be."
  '(integer 1 512))

(defgeneric maximum-length (object))
(defgeneric (setf maximum-length) (length object))
(defgeneric valid-length-p (object &optional sequence))
(defclass maximum-length ()
  ((maximum-length :type maximum-message-length 
                   :reader maximum-length
                   :initarg :length
                   :initarg :maximum-length
                   :initform (error "Maximum length must be specified.")
                   :documentation "Maximum length a sequence may be.")))

(defmethod (setf maximum-length) ((length integer) (object maximum-length))
  (setf (slot-value object 'maximum-length) length))

(defmethod valid-length-p ((length maximum-length) &optional 
                           sequence)
  (length<= sequence (maximum-length length)))

(defgeneric nickname (object))
(defgeneric (setf nickname) (nick object)
  (:documentation
   "Set NICK on OBJECT.

NICK has several constraints. 
  - It must be a character string.
  - It must start with NICKNAME-START-CHARACTER.
  - Type of the second character on must be NICKNAME-CHARACTER.
  - Its length must pass VALID-LENGTH-P.  This uses the MAXIMUM-LENGTH
    slot on OBJECT."))

(defgeneric normalize-nickname (object))

(defclass nickname (maximum-length)
  ((nickname :type nickname-string
             :reader nickname
             :initarg :nick
             :initarg :nickname
             :initform (error "Nickname must be provided.")
             :documentation "IRC user nickname"))
  (:default-initargs :maximum-length 9)) ;Based on rfc2812

(defmethod valid-length-p ((nickname nickname) &optional sequence)
  (call-next-method nickname (or sequence (nickname nickname))))

(defmethod normalize-nickname ((object nickname))
  (normalize-nickname  (slot-value object 'nickname)))

(defmethod normalize-nickname ((nickname string))
  (string-downcase nickname))

(defmethod (setf nickname) ((nickname string) (object nickname))
  (assert (valid-length-p object nickname))
  (setf (slot-value object 'nickname) nickname))

(defclass channel ()
  ())

(defclass mode ()
  ())

(defclass message ()
  ())

(defclass user ()
  ())

(defclass host ()
  ())

(defpackage #:nispbot
  (:use :common-lisp :lift
        :nisp
        :nisp.irc
        :cl-irc :cl-ppcre
        :nispbot-config
        :iterate
        :nisp.ldap
        :nisp.8b-ldap
        :nisp-empty-package
        :nisp-safe
        :nistilities)
  (:shadowing-import-from :iterate :while)
  (:shadowing-import-from :nisp.irc :nickname)
  (:shadowing-import-from :cl-irc :pass))

(in-package :nispbot)

;;; cl-ppcre registers. Do we need this? --nixeagle
(setq *allow-named-registers* t)

(defvar *nispbot*)

(deftype valid-comchar ()
  "Usable characters for irc comchars.

Most anything else in the ASCII set can't be used as they occur as part
of normal conversation and an IRC bot that interferes with that is not
a very friendly bot."
  '(member #\! #\# #\% #\) #\+ #\, #\-
    #\@ #\\ #\] #\_ #\` #\{ #\| #\} #\~))


(defgeneric comchar (object))
(defgeneric (setf comchar) (character object))

(defclass comchar ()
  ((comchar :type valid-comchar
             :reader comchar
             :initarg :char
             :initarg :comchar
             :documentation "Single character that the program responds to."))
  (:documentation "Represents an irc bot comchar.

This is a single character, usually a symbol that the bot responds
to. This class will signal an error if a comchar is not of the type
valid-comchar.")
  (:default-initargs :comchar #\!))

(defmethod (setf comchar) ((char character) (object comchar))
  (declare (type base-char char))
  (setf (slot-value object 'comchar) char))

(defmethod (setf comchar) ((char string) (object comchar))
  (declare (type (base-string 1) char))
  (setf (slot-value object 'comchar)
        (character char)))

(defclass irc-bot (irc:connection comchar)
  ((admin-hosts :accessor irc-bot-admin-hosts
                :initarg :admin-hosts
                :initform nispbot-config::*admin-hosts*)
   (safe :accessor irc-bot-safe
         :initform (make-safe-set))))


(defun make-irc-bot (nick server)
  (connect :nickname nick :connection-type 'irc-bot
           :server server
           :password nispbot-config::*password*))

(defgeneric join-all-channels (instance)
  (:documentation "Join all channels in *channels*. Later this will be expanded to use the connection's channel list."))

(defmethod join-all-channels ((bot irc-bot))
  (mapc
   (lambda (channel)
     (join bot channel))
   nispbot-config::*channels*))

(defgeneric reset-command-hook (instance))
(defmethod reset-command-hook ((bot irc-bot))
  (irc:remove-hooks bot 'irc:irc-privmsg-message)
  (irc:add-hook bot 'irc:irc-privmsg-message #'command-hook))

(defgeneric parse-eval-request (instance message))
(defmethod parse-eval-request ((bot irc-bot) (msg irc-privmsg-message))
  "might want to return our own message type eventually"
  (when (is-eval-request bot msg)
    (subseq (second (arguments msg)) 1)))
(defmethod parse-eval-request ((bot irc-bot) (msg string))
  (when (is-eval-request bot msg)
    (subseq msg 1)))
(defmethod parse-eval-request (bot msg)
  "If we get nil as a message, return nil"
  (declare (ignore bot))
  (when msg
    (error "Failed to parse message ~A" msg))
  nil)
 
(defgeneric is-eval-request (instance message))
(defmethod is-eval-request ((bot irc-bot) (msg irc-privmsg-message))
  (is-eval-request bot (second (arguments msg))))
;;; return something more useful then this...
(defmethod is-eval-request ((bot irc-bot) (msg string))
  (and (< 0 (length msg))
       (eq (char msg 0) (comchar bot))))

(defgeneric safe-eval (instance forms))
(defmethod safe-eval ((message irc:irc-privmsg-message) forms)
  (let ((read-result (safe-read message forms)))
    (multiple-value-bind (res)
                 (let ((*package* (nisp-safe::safe-package
                           (safe-select (irc-bot-safe (connection message))
                                                     (host message)))))
                   (cl::eval read-result))
      res)))
(let ((*package* (find-package :nisp.irc)))
  (defparameter nispbot::*nisp.irc-list*
    '(nisp.irc::channel-string nisp.irc::channel-string-p
      nisp.irc::nickname-string nisp.irc::nickname-string-p)))

(defun nisp-safe::populate-ldap-stuff (safe-package)
  (let ((prior-hello-results '(nil nil nil nil nil nil)))
    (defun safe-closure::hello ()
      (labels ((get-hello (&optional (old-hello ""))
               (if (member old-hello prior-hello-results)
                   (get-hello (nisp.hello:hello))
                   (progn
                     (let ((old-results (nbutlast prior-hello-results)))
                       (setf prior-hello-results
                             (push old-hello old-results)))
                     old-hello))))
        (get-hello (nisp.hello:hello)))))
  (defun safe-closure::ldap-entry (string &optional attrs)
    (one-line-ldif (get-single-entry string :attrs attrs)))
  (defun safe-closure::ircUser (string)
    (safe-closure:ldap-entry (concatenate 'string "uid=" string)))
  (let ((*package* (nisp-safe::safe-package safe-package)))
    (cl::shadowing-import nispbot::*nisp.irc-list* *package*)))

(defun command-hook (message)
  (declare (notinline command-hook))
  "For now lets try to parse just one command"
  (setq 
   nisp-safe::*populate-functions*
        (adjoin 'nisp-safe::populate-ldap-stuff
                nisp-safe::*populate-functions*))
  (let* ((forms (parse-eval-request (connection message) message))
         (admin-request (parse-eval-request (connection message)
                                            forms)))
    (when forms
      (handler-case
          (if (and (member (host message) 
                           (irc-bot-admin-hosts (connection message)) :test #'string=)
                   admin-request)
              ;; User is person running the bot, so allow any lisp to
              ;; be evaluated by that person.
              (trivial-timeout:with-timeout (10)
                (privmsg (connection message)
                         (first (arguments message))
                         (strip-newlines
                          (format nil "~A"
                                  (with-package :nisp.irc
                                    (eval (read-from-string admin-request)))))))
              (trivial-timeout:with-timeout (1)
              ;; Untrusted users, eval their stuff in sandboxes
                (privmsg (connection message)
                         (first (arguments message))
                         (strip-newlines
                          (format nil "~A"
                                  (safe-eval message forms))))))
;        (end-of-file (condition) (values nil condition))
        (error (condition) (privmsg (connection message)
                                    (first (arguments message))
                                    (strip-newlines (format nil "~A" condition))))))))

#+nil
(defun parse-links (string)
  (let ((it ()))
  (cl-ppcre:do-matches-as-strings (var "\\\[\\\[(.*?)\\\]\\\]" string it)
    (describe var)
    (push var it))))

(defun matches-list (reg &rest strings)
  (let ((ret ()))
    (dolist (str strings ret)
      (push
       (second (multiple-value-list
                (cl-ppcre:scan-to-strings reg str))) ret))))


(defmethod safe-read ((msg irc-privmsg-message)
                       (forms string) &optional owner)
  "Read given IRC message in the package corresponding to requesters hostmask"
  (declare (ignore owner))
  (safe-read (irc-bot-safe (connection msg))
             forms
             (host msg)))

(defun pull ()
  "Pull the source from github."
  (trivial-shell:shell-command "git pull"))


;; From nisp-dev-helper
(defun start-nispbot-instance (&optional (nick nispbot-config::*nickname*))
  (setq nispbot::*nispbot* (nispbot::make-irc-bot nick "irc.eighthbit.net"))
  (irc:start-background-message-handler nispbot::*nispbot*)
  (sleep 3) 
  (nispbot::join-all-channels nispbot::*nispbot*)
  (nispbot::reset-command-hook nispbot::*nispbot*))

(defvar *freenode*)
(defun start-freenode-instance ()
  "Quickie to get something up on freenode"
  (setq nispbot::*freenode*
        (irc:connect :connection-type 'nispbot::irc-bot
                     :nickname nispbot-config::*nickname*
                     :server "irc.freenode.net"
                     :password nispbot-config::*freenode-password*))
  (irc:start-background-message-handler nispbot::*freenode*)
  (sleep 3)
  (irc:join nispbot::*freenode* "#botters")
  (nispbot::reset-command-hook nispbot::*freenode*))