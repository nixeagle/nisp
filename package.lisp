(defpackage #:nispbot-config
  (:use :common-lisp)
  (:export *channel* *nickname* *eighthbit*))

(defpackage #:nispbot
  (:use :common-lisp :irc :cl-ppcre
        :nispbot-config))
