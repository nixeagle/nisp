(defpackage #:wikihow
  (:use :cl :drakma :split-sequence :anaphora :alexandria :iterate
        :trivial-timers)
  (:export #:wikihow-rc-status))

(in-package :wikihow)


(defparameter +wikihow-rc-status-uri+
  "http://~A.wikihow.com/Special:RCBuddy?justcount=1")

(defun get-cxml-element-contents (source element)
  (klacks:find-element source element)
  (if (eql :characters (klacks:peek-next source))
      (with-output-to-string (*standard-output*)
        (iter
          (princ (klacks:current-characters source))
          (while (eql :characters (klacks:peek-next source)))))
      "")
  #+ () (lastcar (klacks:serialize-element source (cxml-xmls:make-xmls-builder))))
(defun process-wikihow-status (status-string)
  (mapcar (lambda (pair)
          (let ((split-pair (split-sequence #\= pair)))
            (cons (car split-pair) (ensure-car (cdr split-pair)))))
          (subseq (split-sequence #\Newline status-string) 2)))

(defun wikihow-rc-status (&optional (lang "en"))
  (process-wikihow-status (http-request (format nil +wikihow-rc-status-uri+ lang))))

(defun page-content (name)
  (let ((source (cxml:make-source (http-request (format nil "http://www.wikihow.com/api.php?action=query&prop=revisions&titles=~A&rvprop=content&format=xml"
                                                        name)))))

    (get-cxml-element-contents source "rev")))

(defun site-single-statistic (domain stat-property)
  (let ((source (cxml:make-source (http-request (format nil "http://~A/api.php?action=query&meta=siteinfo&siprop=statistics&format=xml" domain)))))
    (klacks:find-element source "statistics")
    (klacks:get-attribute source stat-property)))

(defun parse-mediawiki-list (start-char text)
  (cl-ppcre:all-matches-as-strings
   (format nil "\\~A.+\\\n" start-char)
   text))


(iter (for (lang . settings) in (cdr (read-from-string (page-content "wikiHow:Whbot/config"))))
      (cl-l10n:with-locale lang
        (let ((cl-l10n::*locale* (car cl-l10n::*locale*)))
          (collect (iter (for (key translation) in settings)
                         (setf (cl-l10n:lookup-resource key)
                               translation))))))


(defvar *rc-quotes* ())
(defparameter +rc-feed-url+ "http://tinyurl.com/2nado7")

(defun format-quote (quote &key count)
  (setq quote (cl-ppcre:regex-replace-all "^\\\*\\\s*|\\\n" quote ""))
  (setq quote (cl-ppcre:regex-replace-all "<<URL>>" quote +rc-feed-url+))
  (cl-ppcre:regex-replace-all "<<COUNT>>" quote (princ-to-string count)))


(defun update-quotes! ()
  (length
   (setq *rc-quotes*
         (wikihow::parse-mediawiki-list "*" (wikihow::page-content "User:Caidoz/whbot")))))

(in-package :nisp.i)

(defun gettext-function (key &rest args)
  (cl-l10n:lookup-resource key :arguments args))

(defmacro gettext (key &rest args)
  `(gettext-function ,(if (symbolp key)
                          `',key
                          key)
                     ,@args))

(defvar *rc-monitor-timer*
  (trivial-timers:make-timer 'check-rc-status :name "Wikihow RC monitor"
                             :thread t))

#+ () (trivial-timers:schedule-timer *rc-monitor-timer* 1
                               :repeat-interval 60)

(let ((total-checks 7)
      (wait-time 5))
 (defun check-rc-status ()
   (let ((total-unpatrolled (assoc-value (wikihow:wikihow-rc-status) "unpatrolled_fa" :test #'equal)))
     (if (zerop (parse-integer total-unpatrolled))
         (setq total-checks 0
               wait-time 5)
         (progn
           (incf total-checks)
           (when (<= wait-time total-checks)
             (setq total-checks 0)
             (incf wait-time)
             (irc:privmsg *whbot* "#wikihow"
                          (wikihow::format-quote (random-elt wikihow::*rc-quotes*) :count
                                                 total-unpatrolled))))))))

(defmacro define-wikihow-command (name &body body)
    `(defmethod handle-nisp-command
       ((tree (eql #-sbcl(network-tree::intern-network-tree-node
                     ,(substitute #\Space #\- (symbol-name name)))
                   #+sbcl ,(substitute #\Space #\- (symbol-name name))))
        (source whbot-bot-connection)
        (user abstract-user)
        (address abstract-target)
        (identity abstract-identity)
        (action abstract-action)
        (content abstract-text-message-content))
       ,@body))

(defmacro define-wikihow-es-command (name &body body)
   `(defmethod handle-nisp-command
       ((tree (eql #-sbcl(network-tree::intern-network-tree-node
                     ,(substitute #\Space #\- (symbol-name name)))
                   #+sbcl ,(substitute #\Space #\- (symbol-name name))))
        (source whbot-bot-connection)
        (user abstract-user)
        (address (eql (irc:find-channel *whbot* "#wikihow-es")))
        (identity abstract-identity)
        (action abstract-action)
        (content abstract-text-message-content))
       ,@body))

#+ ()
(defmethod handle-nisp-command :around
    (tree
     (source whbot-bot-connection)
     (user abstract-user)
     (address (eql (irc:find-channel *whbot* "#wikihow-es")))
     (identity abstract-identity)
     (action abstract-action)
     (content abstract-text-message-content))
                                       (reply (princ-to-string (remaining-parameters)))
  (cl-l10n:with-locale "es"
    (call-next-method)) )

(defmethod route-command :around
    ((source whbot-bot-connection)
     (from irc-user)
     (content irc-message-content)
     (to (eql (irc:find-channel *whbot* "#wikihow-es")))
     (sink whbot-bot-connection))
  (cl-l10n:with-locale "es"
    (call-next-method)))

(define-wikihow-es-command link-nil
  (reply (format-link-wikihow (remaining-parameters) "es")))

(define-wikihow-command stats
  (network-tree::next-node))

(define-wikihow-command stats-article
  (network-tree::next-node))

(define-wikihow-command stats-article-count
  (reply (wikihow::site-single-statistic "en.wikihow.com" "articles")))

(define-wikihow-command link-g
  (reply (format nil "Google: http://google.com/search?q=~A"
                 (substitute #\+ #\Space (remaining-parameters)))))

(define-wikihow-command link-forum
  (let ((post-id (split-sequence #\| (remaining-parameters))))
    (if (every #'string-integer-p post-id)
        (reply (gettext wikihow-bot-cmd-forum (car post-id)
                        (shorturl-is.gd
                         (format nil "http://www.wikihow.com/forum/viewtopic.php?t=~A"
                                 (if (length= 2 post-id)
                                     (format nil "~A#~A" (car post-id) (second post-id))
                                     (car post-id))))))
        (reply (gettext wikihow-bot-cmd-forum-error)))))

(define-wikihow-command help
  (reply (gettext wikihow-bot-cmd-help)))

(define-wikihow-command version
  (reply (gettext wikihow-bot-cmd-version "0.1")))

(define-wikihow-command age
  (reply (gettext wikihow-bot-cmd-age)))

(define-wikihow-command location
  (reply (gettext wikihow-bot-cmd-location "#wikihow-bot")))


(define-wikihow-command quote
  (network-tree::next-node))

(define-wikihow-command quote-update
  (reply (gettext wikihow-bot-fa-rc-quotes (wikihow::update-quotes!))))

(define-wikihow-command quote-test
  (reply (wikihow::format-quote (random-elt wikihow::*rc-quotes*) :count 123456789)))

(define-wikihow-command quote-number
  (reply (wikihow::format-quote (nth (1- (parse-integer (remaining-parameters))) wikihow::*rc-quotes*)
                                :count 123456789)))

(defun format-wikihow-rc-stats (status)
  (gettext wikihow-bot-cmd-rcstats
           (assoc-value status "unpatrolled_total" :test #'equal)
           (assoc-value status "unpatrolled_fa" :test #'equal)
           (assoc-value status "users_editing" :test #'equal)))

(define-wikihow-command rcstats
  (reply (format-wikihow-rc-stats (wikihow:wikihow-rc-status))))
(define-wikihow-es-command rcstats
  (reply (format-wikihow-rc-stats (wikihow:wikihow-rc-status "es"))))

(define-wikihow-command gender
  (reply (random-elt '("I'm a male" "I'm a female"
                       "I'm an alien" "I'm an it"
                       "I'm confused" "I'm Vulcan"
                       "I'm HAL9000"))))

(define-wikihow-command link-nil
  (reply (format-link-wikihow (remaining-parameters))))

(macrolet ((links (&rest langs)
             `(progn
                ,@(mapcar (lambda (lang)
                          `(define-wikihow-command ,(format-symbol t "link-~A" lang)
                             (reply (format-link-wikihow (remaining-parameters) ,lang))))
                        langs))))
  (links "af" "sq" "am" "ar" "hy" "az" "eu" "be" "bn" "bh" "bg" "my" "ca" "chr"
         "zh" "zh-CN" "zh-TW" "hr" "cs" "da" "dv" "nl" "en" "eo" "et" "tl" "fi"
         "fr" "gl" "ka" "de" "el" "gn" "gu" "iw" "hi" "hu" "is" "id" "iu" "it"
         "ja" "kn" "kk" "km" "ko" "ku" "ky" "lo" "lv" "lt" "mk" "ms" "ml" "mt"
         "mr" "mn" "ne" "no" "or" "ps" "fa" "pl" "pt-PT" "pa" "ro" "ru" "sa" "sr"
         "sd" "si" "sk" "sl" "es" "sw" "sv" "tg" "ta" "tl" "te" "th" "bo" "tr"
         "uk" "ur" "uz" "ug" "vi"))

(define-wikihow-command emacs)
(define-wikihow-command markov)
