(in-package :cl-user)
(defpackage #:nisp.irc
  (:use :common-lisp :iterate :metabang-bind :nisp.irc-types :nisp.util-types))
(in-package :nisp.irc)
;;; General generics
(defgeneric convert->string (object)
  ;; Methods specializing on this generic may cease to exist at any moment.
  ;; This exists for prototyping only atm.
  ;;
  ;; An interesting thought is to have this or some other method become
  ;; a setf'able place such that (setf (convert->string OBJECT) "some
  ;; string") would correctly store the contents of "some string" in
  ;; OBJECT.
  (:documentation "Convert OBJECT to a string representation.
This is completely unrelated to the lisp reader."))

;;; Mixins
(defclass username-mixin () ())
(defclass host-mixin () ())
(defclass nickname-mixin () ())
(defgeneric username (object))
(defgeneric nickname (object))
(defgeneric normalize-nickname (object))
(defgeneric host (object))


(defclass abstract-username ()
  ((user :type (or string abstract-username)
         :accessor username 
         :initarg :username
         :initarg :user
         :initform (error "Username must be provided."))))

(defgeneric (setf nickname) (nick object)
  (:documentation
   "Set NICK on OBJECT.

NICK has several constraints. 
  - It must be a character string.
  - It must start with NICKNAME-START-CHARACTER.
  - Type of the second character on must be NICKNAME-CHARACTER.
  - Its length must pass VALID-LENGTH-P.  This uses the MAXIMUM-LENGTH
    slot on OBJECT."))
(defclass abstract-nickname ()
  ((nickname :type (or string abstract-nickname) 
             :accessor nickname
             :initarg :nick
             :initarg :nickname
             :initform (error "Nickname must be provided.")
             :documentation "IRC user nickname")))

;;; Should be a hostname of some sort, what I'm not positive.
(defclass abstract-host ()
  ((host :initarg :host
         :accessor host)))

(defclass abstract-identifier (abstract-nickname abstract-username abstract-host)
  ((user :type abstract-username)
   (nickname :type abstract-nickname)
   (host :type abstract-host)))
(defmethod convert->string ((object abstract-identifier))
  (format nil "~A!~A@~A"
          (nickname (nickname object))
          (username (username object))
          (host (host object))))

(defgeneric maximum-length (object))
(defgeneric (setf maximum-length) (length object))
(defgeneric valid-length-p (object &optional sequence))

(defclass maximum-length ()
  ((maximum-length :type positive-fixnum
                   :accessor maximum-length
                   :initarg :maximum-length
                   :initform (error "Maximum length must be specified.")
                   :documentation "Maximum length a sequence may be.")))

(defclass maximum-message-length (maximum-length)
  ((maximum-length :type (integer 1 512)))
  (:default-initargs :maximum-length 512))

(defclass nickname (abstract-nickname maximum-message-length)
  ((nickname :type nickname-string))
  (:default-initargs :maximum-length 9)) ;Based on rfc2812

(defmethod valid-length-p ((length maximum-length) &optional sequence)
  (length<= sequence (maximum-length length)))

(defmethod valid-length-p ((nickname nickname) &optional sequence)
  (call-next-method nickname (or sequence (nickname nickname))))

(defmethod normalize-nickname ((object abstract-nickname))
  "Lowercase all ASCII letters in OBJECT.

This does _not_ cause [ ] \\ ~ to be translated to { } | ^."
  (normalize-nickname (nickname object)))

(defmethod normalize-nickname ((nickname string))
  (string-downcase nickname))

(defmethod (setf nickname) ((nickname string) (object nickname))
  (assert (valid-length-p object nickname))
  (setf (slot-value object 'nickname) nickname))

(defclass username (abstract-username maximum-message-length)
  ((user :type username-string))
  (:default-initargs :maximum-length 30)) ;Not correct, works for now

(defclass host (abstract-host maximum-message-length)
  ())

(defclass host-address (host)
  ())

(defclass ipv4-host (host-address)
  ((host :type ipv4-address-string))
  (:documentation "Your normal host. 127.0.0.1 and so on."))

(defclass ipv6-host (host-address)
  ())

(defclass host-name (host)
  ((host :type string)))

(defclass rfc-host-name (host-name)
  ((host :documentation "Host with very small charset: [a-zA-Z0-9\\\-\\\.]."))
  (:documentation "More restricted form of hostname, specified in rfc1123."))

(defclass identifier (abstract-identifier)
  ((user :type username)
   (nickname :type nickname)
   (host :type host))
  (:documentation "Represents nickname!user@host"))

(defun make-username (username &rest initargs &key &allow-other-keys)
  "Make username instance unless USERNAME is of type abstract-username."
  (if (typep username 'abstract-username)
      username
      (apply #'make-instance 'username :username username initargs)))

(defun make-nickname (nickname &rest initargs &key &allow-other-keys)
  "Make nickname instance unless NICKNAME is of type abstract-nickname."
  (if (typep nickname 'abstract-nickname)
      nickname
      (apply #'make-instance 'nickname :nickname nickname initargs)))

(defun make-host (host &rest initargs &key &allow-other-keys)
  "Make host instance unless HOST is of type abstract-host."
  (if (typep host 'abstract-host)
      host
      (apply #'make-instance 'host :host host initargs)))

(defmethod initialize-instance ((instance identifier)
                                &rest initargs &key &allow-other-keys)
  (setf (getf initargs :username) 
        (make-username (getf initargs :username)))
  (setf (getf initargs :nickname)
        (make-nickname (getf initargs :nickname)))
  (setf (getf initargs :host)
        (make-host (getf initargs :host)))
  (apply #'call-next-method instance initargs))

(defclass channel ()
  ())

(defclass mode ()
  ())

(defclass message ()
  ())

(in-package :nisp-system)
(defpackage #:nispbot
  (:use :common-lisp :lift
        :nisp
        :nisp.irc-types
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
            :accessor comchar
            :initarg :char
            :initarg :comchar
            :documentation "Single character that the program responds to."))
  (:documentation "Represents an irc bot comchar.

This is a single character, usually a symbol that the bot responds
to. This class will signal an error if a comchar is not of the type
valid-comchar.")
  (:default-initargs :comchar nispbot-config::*comchar*))

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
  (cl:use-package '(:nisp.irc-types) (nisp-safe::safe-package safe-package)))

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

(defvar *sonic*)
(defun start-sonic-instance ()
  (setq nispbot::*sonic*
        (irc:connect :connection-type 'nispbot::irc-bot
                     :nickname "nisp"
                     :server "trogdor.57o9.net"))
  (irc:start-background-message-handler *sonic*)
  (sleep 3)
  (nispbot::reset-command-hook *sonic*))