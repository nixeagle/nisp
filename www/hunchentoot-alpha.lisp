(in-package :cl-user)
(defpackage #:nisp.www
  (:use :cl :hunchentoot :hunchentoot-vhost :nutils :cl-who :parenscript)
  (:shadowing-import-from :parenscript :switch :var)
  (:nicknames :www))
(in-package :nisp.www)

(defun debug-to-irc (arg)
  (print arg)
  #+nisp-devel
  (list (irc:privmsg nisp.i::*devel-bot* "#programming" arg)
                    #+ ()   (irc:privmsg nisp.i::*freenode* "#botters" arg))
  #+nisp-vps
  (irc:privmsg nisp.i::*9b* "#programming" arg))

(defun debug-to-bikcmp (arg)
  #+nisp-vps
  (irc:privmsg nisp.i::*bikcmp* "#bots" arg))

(setq *prologue* "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
  (setf (html-mode) :SGML)
(setq *js-string-delimiter* #\")

(defmacro with-html (&body body)
  "Do with-html-output-to-string with BODY.

This is the sensible and sane default that hunchentoot does not seem to
provide on its own."
  `(progn

     (with-html-output-to-string (*standard-output* nil #+ () :prologue #+ () t)
       ((:html ;:lang "en-US"
               ;:xmlns "http://www.w3.org/1999/xhtml"
               )
        ,@body))))

(defparameter *nisp-port* 80
  "Default port for alpha-site.")

(defclass nisp-site-acceptor (acceptor)
  ()
  (:default-initargs :port *nisp-port*)
  (:documentation "Alpha site has its own acceptor so that testing hooks
can specialize on the acceptor class and not mess with the normal
hunchentoot acceptor."))

(defclass nisp-1337-acceptor (nisp-site-acceptor)
  ()
  (:default-initargs :port 1337 :name :leet)
  (:documentation "Messing around on port 1337 mostly. This is just a
  silly and near worthless pun with numbers meaning 'leet'."))

(defvar *nisp-1337-acceptor* (make-instance 'nisp-1337-acceptor))
(defvar *nisp-8080-acceptor* (make-instance 'nisp-1337-acceptor :port 8080 :name :chirps))
(defvar *nisp-6667-acceptor* (make-instance 'nisp-1337-acceptor :port 6667 :name :chirps))

#+ nisp-devel (progn
  (defvar *nisp-started-6667-acceptor* (start *nisp-6667-acceptor*))
  (defvar *nisp-started-8080-acceptor* (start *nisp-8080-acceptor*)))
(defvar *nisp-acceptor* (make-instance 'nisp-site-acceptor)
  ;; Note there is an oddity that I cannot stop one of these and then
  ;; start it and expect it to work as I would normally intend. Calling
  ;; REINITIALIZE-INSTANCE on the instance did not do anything useful
  ;; either.
  ;;
  ;; So to restart the accepter, call (stop *alpha-acceptor*)
  ;; followed by: (setq *alpha-acceptor* (make-instance ...))
  ;; and then call (start *alpha-acceptor*).
  "Default acceptor.")


(defparameter *commits*
  (hunchentoot-vhost:make-virtual-host "commits"
                                       '(#+nisp-vps "commits.nixeagle.net"
                                         #+nisp-devel "pri.wiki.james.nixeagle.net")
                                       :server
                                       #+nisp-vps
                                       (assoc-value cl-user::*root-ports* 80)
                                       #+nisp-devel
                                       *nisp-started-8080-acceptor* #+ ()
                                       (list
                                             *nisp-started-1337-acceptor*
                                             *nisp-started-6667-acceptor*) ))

(defvar *nisp-started-1337-acceptor*
  (when (eq (nisp.global::system-keyword) :nisp-devel)
    (start *nisp-1337-acceptor*)))



(defvar *nisp-last-request* nil
  "Contents of the last hunchentoot request.")

;;; Modifications/around methods on hunchentoot generics.
(defmethod handle-request :before ((acceptor nisp-site-acceptor) request)
  "Set *ALPHA-LAST-REQUEST* to REQUEST."
  (setq *nisp-last-request* request)
  #+ () (setq *alpha-acceptor* *acceptor*))


;;; dispatching things ------------------------------
(defun default-handler ()
  "Called if nothing else matches."
  (log-message :info "Default dispatch called on nisp-site, script: ~A"
               (script-name*))
  "Nothing here!")

(defvar *old-default-handler* *default-handler*
  "Value of default-handler before we mess with it.")

(defun set-default-handler! (&optional reset)
  (if reset
      (setq *default-handler* *old-default-handler*)
      (setq *default-handler* 'default-handler)))

(defvar *github-request*)
;;; Just messing with github hooks
(defun decode-github-hook (request)
  (setq *github-request* request)
  (json:decode-json-from-string
   (cdr (assoc "payload" (post-parameters request) :test #'equalp))))

(defun github-compare-view (repository before after)
  (format nil "~A/compare/~A...~A"
          repository before after))

(defun github-compare-view-from-payload (payload-alist)
  (declare (type list payload-alist))
  (github-compare-view
   (assoc-value (assoc-value payload-alist :repository) :url)
   (assoc-value payload-alist :before)
   (assoc-value payload-alist :after)))

(defun github-address-p (octets &key
                         (octets-begin (list 207 97 227 224))
                         (octets-end (list 207 97 227 255)))
  (every #'<= octets-begin octets octets-end))
(defvar *foobar*)

(defun format-github-commit-message (alist-message)
  (setq *foobar* alist-message)
  (let ((commits (assoc-value alist-message :commits))
        (repository (assoc-value alist-message :repository)))
    (apply #'list
           (format nil "~A/~A: ~D new commits, compare view at <~A>. ~:[~;~D outstanding issues.~]"
                   (assoc-value (assoc-value repository :owner) :name)
                   (assoc-value repository :name)
                   (length commits)
                   (nisp.i::shorturl-is.gd
                    (github-compare-view-from-payload
                     alist-message))
                   (assoc-value repository :has-issues)
                   (assoc-value repository :open-issues))
           (loop for commit in commits
              collect
                (format nil "~A:~
                             ~@[ Added ~{~A~^, ~}.~]~
                             ~@[ Modified ~{~A~^, ~}.~]~
                             ~@[ Removed ~{~A~^, ~}.~] ~
                             With summary: ~A"
                      (assoc-value (assoc-value commit :author) :name)
                      (assoc-value commit :added)
                      (assoc-value commit :modified)
                      (assoc-value commit :removed)
                      (subseq (assoc-value commit :message) 0 (position #\NewLine (assoc-value commit :message))))))))

(define-easy-virtual-handler *commits*
    (commits-github :uri "/github") (payload)
  (setf *github-request* *request*)
  (if (github-address-p (mapcar #'parse-integer
                                (split-sequence:split-sequence #\. (remote-addr*))))
      (let* ((sexp  (json:decode-json-from-string payload))
            (owner (assoc-value
                    (assoc-value
                     (assoc-value sexp :repository) :owner) :name)))
        (loop for commit in (format-github-commit-message sexp)
           do (unless (string= "aj00200" owner)
                (debug-to-irc commit))
             (when (or (string= "nixeagle" owner)
                       (string= "aj00200" owner))
             (debug-to-bikcmp commit)))
        #+ ()        (debug-to-irc (format nil "Compare view push: ~A"
                                        ;(assoc-value (cdar sexp) :name)
                                           (nisp.i::shorturl-is.gd
                                            (github-compare-view-from-payload
                                             sexp)))))
      "You are not github, bye now!"))

(define-easy-handler (github-hook :uri "/github-hook")
    (payload)
  (setf *github-request* *request*)
  (if (github-address-p (mapcar #'parse-integer
                                (split-sequence:split-sequence #\. (remote-addr*))))
      (let ((sexp  (json:decode-json-from-string payload)))
        (loop for commit in (format-github-commit-message sexp)
           do (debug-to-irc commit)))
      "You are not github, bye now!"))

(defun random-link (link-text &optional (stream *standard-output*))
  (declare (type string link-text))
  (with-html-output (stream)
    ((:a :title "Try a new random number!"
         :style "color: pink;"
         :href (format nil "http://i.nixeagle.net:1337/test?echo=~A"
                       (random 2)))
     (str link-text))))

(define-easy-handler (random-number-test :uri "/random-number") ()
  (princ-to-string (random 2)))
(define-easy-handler (echo-test :uri "/test") ()
  (with-html
    (:head (:title "Lisp web development test page"))
    (:body (:ol (iter:iter (iter:repeat 10)
                           (htm (:li (random-link "Reload"))))))))

(define-easy-handler (js-source :uri "/test/js.js") ()
  (setf (content-type*) "text/javascript")
  (ps (let ((total-flips))
        (defun coin-toss-callback (&optional (flips 10) (total-heads 0))
          (unless total-flips
            (setq total-flips flips))
          (if (< 0 flips)
              (simple-ajax-request
               "/random-number"
               (lambda (res code text)
                 (when (= 200 code)
                   (coin-toss-callback (- flips 1)
                                       (+ total-heads (parse-int text))))))
              (alert (+ total-heads " heads out of " total-flips " flips.")))))
      (defun simple-ajax-request (resource-string func &optional (send-method "POST"))
        "Do a request to RESOURCE-STRING handled by FUNC.

FUNC takes two arguments which is the result object and the response text.
This function is only called if the ajax request completes.

The optional SEND-METHOD parameter may be either 'POST' or 'GET'
defaulting to 'POST'."
        (let ((result
               (cond
                 ((@ window -X-M-L-Http-Request) (new -X-M-L-Http-Request))
                 ((@ window -active-x-object)
                  (new (-active-x-object "Microsoft.XMLHTTP"))))))
          (setf (@ result onreadystatechange)
                (lambda ()
                  (if (and (= 4 (@ result ready-state)))
                      (funcall func result (@ result status)
                               (@ result response-text)))))
          (chain result (open send-method resource-string t))
          (chain result (send null))))
      (defun do-ajax-request ()
        (simple-ajax-request
         "/random-number"
         (lambda (result result-status result-text)
           (when (= 200 result-status)
             (setf (@ document body children 0 text-content) result-text)))))))

(define-easy-handler (js-test :uri "/test/js") ()
  (with-html
    (:head
     (:title "JS lisp tests")
     (:script :src "./js.js" :type "text/javascript"))
    (:body
     ((:p :name "ajax") "Hit the 3rd link down to put something other then text here.")
     (:ul
      (:li
       ((:a :href "#"
            :onclick (ps (coin-toss-callback)))
        "javascript ajax cointoss"))
      (:li
       ((:a :href "#" :onclick
            (ps (do-ajax-request)))
        "javascript ajax request to random-number"))))))


(defun ensure-no-leading-single-quote (string)
  "Append a leading ' to STRING if there is not already one."
  (declare (type string string))
  (if (char= (char string 0) #\')
      (subseq string 1)
      string))
(defgeneric eval-in-emacs (form &optional nowait))
(defmethod eval-in-emacs ((form string) &optional nowait)
  (eval-in-emacs (read-from-string (ensure-no-leading-single-quote form))
                 nowait))
(defmethod eval-in-emacs (form &optional nowait)
  (let ((swank::*emacs-connection* (swank::default-connection)))
    (swank::eval-in-emacs form nowait)))

(defparameter *allowed-emacs-buffers*
  `("*scratch*" "*slime-scratch*" "boot.lisp"))

(define-easy-handler (pic-hook :uri "/pic") ()
  (handle-static-file "/home/james/2010-05-26-000022_1366x768_scrot.png"))

(define-easy-handler (emacs-buffer-showing-hook :uri "/emacs/buffer")
    ((name :init-form "*scratch*"))
  (setf (content-type*) "text")
  (when (member name *allowed-emacs-buffers* :test #'equal)
    (eval-in-emacs (format nil "(htmlize-buffer-to-string \"~A\")"
                           (remove #\" name)))))


(defun string-escape-double-quote (string)
  (with-output-to-string (*standard-output*)
    (loop for char across string
       when (eql char #\") do (princ #\\)
       do (princ char))))

(defun string-escape-single-quote (string)
  (with-output-to-string (*standard-output*)
    (loop for char across string
       when (eql char #\') do (princ #\\)
       do (princ char))))

(defun emacs-one-string-arg-function (function-name)
  "Returns function to call FUNCTION-NAME with one string arg."
  (declare (type (or symbol string) function-name))
  (let ((format-string (format nil "(~(~A~) \"~~A\")" function-name)))
    (lambda (string-arg &aux
             (string-arg (string-escape-double-quote string-arg)))
      (declare (type string string-arg))
      (eval-in-emacs (format nil format-string string-arg)))))

(defvar *short-buffers* (list "*slime-repl sbcl*"
                              "*ielm*"
                              "*ioke*"))
(defun htmlize-buffer (buffer-name)
  "Get the html version of BUFFER-NAME"
  (if (member buffer-name *short-buffers* :test 'equal)
      (funcall (emacs-one-string-arg-function
                "htmlize-repl-buffer-to-string-body-only") buffer-name)
      (funcall (emacs-one-string-arg-function
                "htmlize-buffer-to-string-body-only")
               buffer-name)))

(defun emacs-buffer-modified-tick (buffer-name)
  (declare (ignore buffer-name))
  (eval-in-emacs "nisp-live-buffer-current-tick"))

(defun crappy-htmlize-no-body (&optional (live-buffer *live-emacs-buffer*))
  (let* ((buf (htmlize-buffer live-buffer))
         (end-pos (position #\Newline buf :from-end t)))

    (let* ((halfway-buf
            (subseq buf (position #\Newline buf :start 143)
                    (position #\Newline buf :from-end t :end (- end-pos 10))))
           (end-body-start (search "</head>" halfway-buf)))
      (concatenate 'string (subseq halfway-buf 0 end-body-start)
                   (subseq halfway-buf
                           (position #\newline halfway-buf
                                     :start (+ end-body-start 9)))))))
(defun stext ()
  *htmlized-buffer-text*)

(defun spoint ()
  (search "<span class=\"nisp-live-buffer-point\">"
          (stext)))

(defun endpos ()
  (position #\newline (stext) :start (spoint)))

(defun startpos ()
  (position #\newline (stext) :end (endpos) :from-end t))

(defun crappy-html-cleanup (buf)
  (if (< 143 (length buf))
      (let* ((end-pos (position #\Newline buf :from-end t))
             (halfway-buf
              (subseq buf (position #\Newline buf :start 143)
                      (position #\Newline buf :from-end t :end (- end-pos 10))))
             (end-body-start (search "</head>" halfway-buf)))
        (concatenate 'string (subseq halfway-buf 0 end-body-start)
                     (subseq halfway-buf
                             (position #\newline halfway-buf
                                       :start (+ end-body-start 9)))))
      ""))

(defvar *live-emacs-buffer* "*scratch*")
(defvar *htmlized-buffer-text* "")
(defvar *update-count* 10000)
(defvar *live-emacs-buffer-update-speed* 1/16)
(define-easy-handler (emacs-buffering-showing-slow-update-cgi
                      :uri "/emacs/slow-buffer-update")
    ((tick :init-form -1 :parameter-type 'integer)
     (test :init-form "I redefined it on the fly and hit C-c C-c"))
  (when (boundp '*reply*)
    (no-cache))
  (if (= -1 tick)
      (progn (setq test "New connection!")
             (sleep 2))
      (sleep *live-emacs-buffer-update-speed*))
  (loop do
       (let ((current-tick *update-count*))
         (if (= tick current-tick)
             (sleep *live-emacs-buffer-update-speed*)
             (let ((htmlized-string (crappy-html-cleanup *htmlized-buffer-text*)))
               (return
                 (let ((*js-string-delimiter* #\'))
                   (with-html-output-to-string (*standard-output*)
                     (:html
                      (:head
                       (:script
                        (ps-to-stream *standard-output*
                          (defun slow-update ()
                            ((@ window top frames main push-buffer-event)
                             (lisp htmlized-string)
                             (lisp (concatenate 'string
                                                "Buffer: "
                                                *live-emacs-buffer*
                                                " Max pull rate: "
                                                (princ-to-string *live-emacs-buffer-update-speed*)
                                                "s"
                                                "  String Length: "
                                                (princ-to-string (length htmlized-string))
                                                " chars.")))
                            (setf (@ window top frames slow_update location)
                                  (lisp (format nil "/emacs/slow-buffer-update?tick=~A"
                                                current-tick)))))))
                      ((:body :onload "slowUpdate();")
                       (:p (format t "current-buffer-tick: ~A" current-tick))
                       (:p (format t "test: ~A" *live-emacs-buffer*))))))))))))


(define-easy-handler (emacs-buffering-showing-slow-update-js
                      :uri "/js/emacs/push-buffer-event.js")
    ()
  (when (boundp '*reply*)
    (setf (content-type*) "text/javascript"))
  (ps (defun push-buffer-event (value info)
        (setf (@ ((@ document get-element-by-id) "buffer_info") text-content)
              info)
        (setf (@ ((@ document get-element-by-id) "buffer_text") inner-h-t-m-l)
              value)
        (return value))))


(define-easy-handler (emacs-buffer-showing-slowload-main
                      :uri "/tmain") ()
  (with-html-output-to-string (*standard-output*)
    (:html
     (:head
      (:script :src "/js/emacs/push-buffer-event.js" :type "text/javascript"))
     (:body
      (:p :id "buffer_info" "Buffer information here")
      (:p :id "buffer_text"  (:noscript "Turn your scripts on and make sure to allow i.nixeagle.net.")
          "If you don't see something here after 2 or 3 seconds please ping nixeagle as you found a boog.")))))
(define-easy-handler (emacs-buffer-showing-slowload-hook
                      :uri "/t")
    ()

  (with-html-output-to-string (*standard-output* nil :PROLOGUE t)
    (:html
     (:head
      (:title "Emacs paste buffers"))

     ((:frameset :rows "*,0")
      (:frame :src "/tmain" :name "main" :id "main")
      (:frame :src "/emacs/slow-buffer-update" :name "slow_update"
              :id "slow_update"))
     (:noframes
      (:p "Would be ideal to show the buffer anyway.")))))

(defun full-describe-for-emacs (name)
  (with-output-to-string (*standard-output*)
    (multiple-value-bind (symbol flag)
        (swank::parse-symbol name)
      (swank:describe-definition-for-emacs "DEFPARAMETER" :macro)
      (when flag
        (loop for x in (swank-backend:describe-symbol-for-emacs symbol)
           do (format t "~A" (if (keywordp x)
                                 (format nil "~%~A:~%" x)
                                 x)))))))



#+ () (define-easy-handler (9b-commit-hook :uri "/9b/commit-hook")
    (name summary)
  )
#+ ()
(debug-to-irc (format nil "Compare view for ~A push: ~A"
                      (assoc-value (cdar sexp) :name)
                        (nisp.i::shorturl-is.gd
                         (github-compare-view-from-payload
                          sexp))))

;;; End hunchentoot-alpha.lisp (for magit/git)
