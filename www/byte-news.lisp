(defpackage #:nixeagle.local-time.helpers
  (:use :cl :local-time)
  (:documentation "Several functions and macros that should be in the
  local-time package but are not. Should be submitted at some point
  upstream, but these are needed now.")
  (:export #:with-local-time-reader))
(in-package :nixeagle.local-time.helpers)

(defun call-with-local-time-reader (thunk &optional (base-readtable *readtable*))
  "Call THUNK with local-time read macro extended copy of BASE-READTABLE."
  (let ((*readtable* (copy-readtable base-readtable)))
    (enable-read-macros)
    (funcall thunk)))

(defmacro with-local-time-reader (&body body)
  "Bind `local-time' reader macros over BODY."
  `(call-with-local-time-reader (lambda () ,@body)))

(defpackage #:ninthbit.www.news
  (:use :cl :iterate :cl-who)
  (:import-from :www #:*9b* #:with-html))
(in-package :ninthbit.www.news)

(defstruct byte-news
  name news (time (local-time:now)))

(defvar *byte-news* nil
  "Feed of recent news from bytes and bits!")

(defun add-news-item (nick news-text)
  (declare (string nick news-text))
  (push (make-byte-news :name nick :news news-text) *byte-news*))

(defun save-byte-news ()
  (with-open-file (stream "news.dat" :direction :output)
    (print *byte-news* stream)))

(defun read-byte-news ()
  (with-open-file (stream "news.dat" :direction :input)
    (setq *byte-news* (read stream))))

(hunchentoot-vhost:define-easy-virtual-handler *9b*
    (byte-news-feed :uri "/news") ()
  (with-html
    (:head
     (:title "9b news"))
    (:body
     (iter (for news :in *byte-news*)
           (htm (:p (fmt "~A :: ~A - ~A"
                         (local-time:to-rfc3339-timestring (byte-news-time news))
                         (byte-news-name news)
                         (byte-news-news news))))))))

#+:nisp-devel                           ; don't do elsewhere yet.
(closer-mop:defmethod nisp.i::handle-nisp-command
    ((nisp.i::tree (eql "UPDATE")) (source nisp.i::9b-dev-bot-connection)
     (user nisp.i::abstract-user)
     (address nisp.i::abstract-target)
     (identity nisp.i::abstract-identity)
     (action nisp.i::abstract-action)
     (content nisp.i::abstract-text-message-content))
  (add-news-item (nisp.i::nickname user)
                 (nisp.i::remaining-parameters))
  (nisp.i::reply "Reported \"~A\" to the 9b beta news feed."
                 (nisp.i::remaining-parameters)))
