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
  (sor (irc:normalize-nickname connection nickname)
       (gethash it (irc:users channel))
       (when (string= it (nickname (irc:user connection)))
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
  (:method-class handle-nisp-command-method)
  (:method-combination nisp-standard-method-combination:nisp-standard))

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
                 (to target) (content string) &rest additional-args)
  (declare (ignore action))
  "Default action for irc is to privmsg"
  (privmsg sink to (remove #\Newline (apply #'format nil content additional-args))))
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


(defun parse-link (line)
  (nth-value 1 (cl-ppcre:scan-to-strings "\\\[\\\[(?:([^:]+):)?(.*?)\\\]\\\]" line)))

(defgeneric route-command (source from content to sink))
(defmethod route-command :around (source from content to sink)
  "Time how long calls and what parems were used and put this timing data
  in a list."
  (if *debug*
      (call-next-method)
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
         (push-new-time (call-next-method)))))))

(defvar *command-args*)
(defmethod route-command  ((source abstract-data-source)
                           (from abstract-from)
                           (content abstract-message-content)
                           (to abstract-target)
                           (sink abstract-data-sink))
  (declare (ignore sink))
  (acond
    ((commandp content)
     (setq *command-args* (list (message content) source (name from)
                                to (make-instance 'abstract-identity)
                                (make-instance 'abstract-action) content))
     (handle-nisp-command (message content) source (name from)
                          to (make-instance 'abstract-identity)
                          (make-instance 'abstract-action) content))
    ((parse-link (message content))
     (handle-nisp-command (format nil "link ~A ~A" (aref it 0) (aref it 1))
                          source (name from)
                          to (make-instance 'abstract-identity)
                          (make-instance 'abstract-action) content))))



#+ () (defmethod handle-nisp-command ((tree (eql "say"))
                                (source abstract-data-source)
                                (user abstract-user)
                                (address abstract-address)

                                (identity abstract-identity)
                                (action abstract-action)
                                (content abstract-message-content))
  (privmsg source address (network-tree::remaining-parameters)))

(defun string-integer-p (string)
  (= (length string) (nth-value 1 (parse-integer string :junk-allowed t))))



(define-simple-command lag
  (reply (format nil "~A seconds."
                 (float (/ (random 10000) (random 10000))))))
(define-simple-command emacs
  (network-tree::next-node))
(define-simple-command source
  (reply "I'm written in common lisp by nixeagle. You can find my source at <http://github.com/nixeagle/nisp/tree/master/irc-bot/>."))
(define-simple-command emacs-live
  (reply "Nixeagle's live emacs buffer is at http://i.nixeagle.net:1337/t or http://i.nixeagle.net:8080/t . For best results please view in firefox or chrome. Opera won't even show the text (source unknown). IE gets some silly runtime error and fixing it is not a priority."))
(define-simple-command say (reply (remaining-parameters)))

(define-simple-command link
  (handler-case (network-tree::next-node)
    (error (condition)
      (network-tree::next-node
       (apply #'format nil "nil ~A:~A"
              (multiple-value-list (network-tree::first-command-word (remaining-parameters))))))))

(defmethod handle-nisp-command
    ((tree (eql "link")) (source connection) (user abstract-user)
     (address bot-channel) (identity abstract-identity)
     (action abstract-action) (content abstract-text-message-content))
  (unless #+nisp-vps (or (gethash "whbot-dev" (irc:users address) nil)
                         (gethash "nisp-devel" (irc:users address) nil))
          #-nisp-vps nil
          (handler-case (nisp.network-tree::next-node)
            (error (condition)
              (declare (ignore condition))
              (nisp.network-tree::next-node
               (apply #'format nil "nil ~A:~A"
                      (multiple-value-list
                       (nisp.network-tree::first-command-word
                        (remaining-parameters)))))))))

(defun format-euler-problem-url-text (id)
  (let ((url (format nil "http://projecteuler.net/index.php?section=problems&id=~A" id)))
    (let ((it (dom:get-elements-by-tag-name (chtml:parse (drakma:http-request url) (rune-dom:make-dom-builder)) "p")))
      (format nil "Project Euler problem #~A: ~A <~A>"
              id
              (dom:data (dom:first-child (aref it (1- (fill-pointer it)))))
              (shorturl-is.gd url)))))


(define-simple-command link-euler
  (if (string-integer-p (remaining-parameters))
      (reply (format-euler-problem-url-text (remaining-parameters)))
      (reply (format nil "Project Euler profile for ~A: ~A" (remaining-parameters)
                     (shorturl-is.gd (format nil "http://projecteuler.net/index.php?section=profile&profile=~A" (remaining-parameters)))))) )

(defun format-link-wikipedia (params)
  (format nil "Wikipedia article ~A: ~A" params
          (format nil "http://en.wikipedia.org/wiki/~A"
                  params)))
(define-simple-command link-wikipedia
  (reply (format-link-wikipedia (remaining-parameters))))
(define-simple-command link-wiki
  (reply (format-link-wikipedia (remaining-parameters))))
(define-simple-command link-nil
  (reply (format-link-wikipedia (remaining-parameters))))

(define-simple-command shorturl
  (reply (shorturl-is.gd (remaining-parameters))))

(defvar *nist-compsci-dictionary* (make-hash-table :test #'equalp))

(iter (for x in-vector (dom:get-elements-by-tag-name
                        (chtml:parse (drakma:http-request "http://www.itl.nist.gov/div897/sqg/dads/ui.html")
                                     (rune-dom:make-dom-builder)) "a"))
      (when (search "HTML/" (dom:get-attribute x "href") :end2 5)
        (when (dom:text-node-p (dom:first-child x))
          (setf (gethash (dom:data (dom:first-child x)) *nist-compsci-dictionary*)
                (concatenate 'string "http://www.itl.nist.gov/div897/sqg/dads/"
                             (dom:get-attribute x "href"))))))

(define-simple-command link-nist
  (reply (gethash (remaining-parameters) *nist-compsci-dictionary*
                  (format nil "Requested page ~A does not exist in my index."
                          (remaining-parameters)))))

(define-simple-command link-cpan
  (reply (format nil "CPAN: http://search.cpan.org/search?mode=all&query=~A"
                 (remaining-parameters))))

(define-simple-command link-emacs
  (reply (format nil "Emacwiki: ~A"
                 (shorturl-is.gd (format nil "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q=~A&sa=Search" (substitute #\+ #\Space (remaining-parameters)))))))

(define-simple-command link-wk
  (reply (format nil "wiktionary: http://en.wiktionary.org/wiki/~A"
                 (remaining-parameters))))

(defun string-capitalize-only (string)
  (cl-ppcre:regex-replace-all "\\\b(.)"
                              string
                              (lambda (target-string start end match-start match-end reg-starts reg-ends)
                                (declare (ignore start end reg-ends reg-starts))
                                (string-upcase (subseq target-string match-start match-end)))))

(defun format-link-wikihow (params &optional lang)
  (let ((article (substitute #\- #\Space params)))
    (format nil "wikiHow page ~A: ~A"
            (substitute #\Space #\- article)
            (if lang
                (format nil "http://~A.wikihow.com/~A"
                        lang article)
                (format nil "http://wikihow.com/~A"
                        article)))))

(define-simple-command link-wikihow
  (reply (format-link-wikihow (remaining-parameters))))


(let ((count 0))
  (define-simple-command link-rfc
    (incf count)
    (reply (if (string-integer-p (remaining-parameters))
               (format nil "RFC: http://www.ietf.org/rfc/rfc~A" (remaining-parameters))
               "RFCs are referred to by number")))

  (define-simple-command help
    (incf count)
    (reply (format nil "Help? what help. Count: ~A" count))))

(defun random-up-or-down (&optional (magnitude 1))
  (if (zerop (random 2))
      (- magnitude)
      magnitude))

(defun generate-markov-sequence (&key (initial 0) (range 10))
  "Demo markov to get the idea of it."
  (iter (for up-or-down = (random-up-or-down))
        (for n initially initial then (+ n up-or-down))
        (until (or (> n (+ initial range)) (< n (+ initial (- range)))))
        (collect (list n up-or-down))))


(define-simple-command markov
  (reply (remove #\Newline
                 (format nil "~A" (generate-markov-sequence
                                   :initial 0 :range 3)))))


(defvar *states* (list :positive-even :positive-odd :negative-odd :negative-even :zero))
;;=> *STATES*

(defvar *learning-rate* 1/10
  "Rate that we adjust things based on a reward.")

(defvar *actions* (list (constantly "Probably")
                        (constantly "Bye")
                        (constantly "Hi")
                        (constantly "Interesting")
                        (constantly "ZzzZzzZzzZzz")))


(flet ((generate-action-list (&optional (initial-score 5) (action-set *actions*))
         (mapcar (lambda (x) (cons x initial-score)) action-set)))
  (defparameter *lookup-table* (alist-hash-table
                                `((:positive-even . ,(generate-action-list))
                                  (:positive-odd . ,(generate-action-list))
                                  (:zero . ,(generate-action-list))
                                  (:negative-even . ,(generate-action-list))
                                  (:negative-odd . ,(generate-action-list))))
    "We default the score for every entry to 5. This is chosen by the
    programmer. We have to start with some default. Our default is flat
    between the range 0 and 10. These are arbitrary values"))

(defun state-action-score (state action)
  "Scoring for a particular state/action pair."
  (cdr (assoc action (gethash state *lookup-table*))))


(defun (setf state-action-score) (score state action)
  (setf (cdr (assoc action (gethash state *lookup-table*)))
        score))


(defun state-learning-rate (state action)
  *learning-rate*)

(defun state-action-max-score (state action)
  10)

(defun compute-new-score (state action reward)
  (+ (state-action-score state action)
     (* (state-learning-rate state action)
        (+ reward
           (- (state-action-score state action))))))

(defun compute-state (number-input)
  (let ((number-input (if (stringp number-input)
                          (parse-integer number-input)
                          number-input)))
    (cond
      ((zerop number-input) :zero)
      ((and (> 0 number-input) (evenp number-input)) :negative-even)
      ((and (> 0 number-input) (oddp number-input)) :negative-odd)
      ((evenp number-input) :positive-even)
      (t :positive-odd))))


(defun flux (normal-score)
  (+ normal-score (/ (random 150) 100)))

(defun compute-possible-actions (string-integer)
  (aif (parse-integer string-integer :junk-allowed t)
       (mapcar (lambda (action)
                 (cons (state-action-score
                         (compute-state it) action) action)) *actions*)
       "I expect integers only! Your brainzzzzzzzz plox!"))

(defun add-flux (action-list)
  (mapcar (lambda (action-score-pair)
            (cons (flux (car action-score-pair))
                  (cdr action-score-pair)))
          action-list))

(defun compute-best-action (action-list)
  (sort action-list #'> :key #'car))

(defun handle-ai (string-integer)
  (aif (string-integer-p string-integer)
       (car (compute-best-action (add-flux (compute-possible-actions string-integer))))
       "I expect integers only!"))

(let ((input "")
      (last-score 0)
      (last-action nil)
      (last-state nil))
  (define-simple-command ai
    (if (string-integer-p (remaining-parameters))
        (progn
          (setq input (remaining-parameters))
          (destructuring-bind (score . action) (handle-ai (remaining-parameters))
            (setq last-action action
                  last-score score
                  last-state (compute-state (parse-integer (remaining-parameters))))
            (reply (remove #\Newline
                           (funcall action)))))
        (network-tree::next-node)))
  (define-simple-command ai-test-state
    (reply (princ-to-string (compute-state (parse-integer (remaining-parameters))))))
  (define-simple-command ai-previnfo
    (reply (list input last-score last-action)))
  (define-simple-command ai-test
    (network-tree::next-node))
  (define-simple-command ai-test-reward
    (if (string-integer-p (remaining-parameters))
        (reply (format nil "New score for state ~A and action ~A would be ~A."
                       last-state
                       (funcall last-action)
                       (float (compute-new-score last-state last-action (parse-integer (remaining-parameters))))))
        (reply "Come on! Integers rock!")))
  (flet ((format-reward-reply (score)
           (format nil "Score for state ~A saying ~A is now ~A."
                   last-state
                   (funcall last-action)
                   (float (reward-ai last-state last-action score)))))
  (define-simple-command ai-trout
    (reply (format-reward-reply 0)))
  (define-simple-command ai-cookie
    (reply (format-reward-reply 10)))))

(defun reward-ai (state action amount)
  (setf (state-action-score state action)
        (compute-new-score state action amount)))
(define-simple-command test
  (network-tree::next-node))

(define-simple-command test-action
  (reply (funcall (nth (random 5) *actions*))))

(define-simple-command test-tokenize
  (network-tree::next-node))

#+ () (define-simple-command test-tokenize-word
  (with-input-from-string (s (remaining-parameters))
    (reply (nisp.tokenize::parse-word s))))

(define-simple-command tokenize
  (reply (format nil "~S" (nisp.tokenize:tokenize-string (remaining-parameters)))))

(define-simple-command regex
  (let ((match-array (nth-value 1 (cl-ppcre:scan-to-strings "/(.+?)/ (.+)"  (remaining-parameters)))))
  (reply
   (multiple-value-bind (start end reg-start reg-end)
       (cl-ppcre:scan
        (aref match-array 0)
        (aref match-array 1))
     (if start
         (format nil "match: (~A, ~A)~A" start end
                 (if (length= 0 reg-start)
                     ""
                     (format nil " Groups: ~{~A~}"
                             (mapcar #'list (coerce reg-start 'list)
                                     (coerce reg-end 'list)))))
         "Does not match")))))
(define-simple-command test-wordinfo
  (aif (gethash (remaining-parameters) wiktionary::*dictionary*)
       (reply (format nil "~A types: ~A"
                      (wiktionary::word-name it)
                      (wiktionary::word-pos it)))
       ;(reply "borked!")
       (reply "I don't know!")))

(define-simple-command word
  (network-tree::next-node))

(define-simple-command word-pos
  (multiple-value-bind (pos-list found?)
      (wiktionary:lookup-pos (remaining-parameters))
    (if found?
        (reply (format nil "~A" pos-list))
        (reply "I can't find that word!"))))

#+ ()  (www::define-easy-handler (www::ai-demonstration :uri "/ai")
      ((number :init-form 0 :parameter-type 'integer))
    (princ-to-string (funcall (cdr (handle-ai (princ-to-string number))))))


;;; END
