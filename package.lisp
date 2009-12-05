(defpackage #:nispbot-config
  (:use :common-lisp))

(defpackage #:nispbot
  (:use :common-lisp :irc :cl-ppcre
        :nispbot-config))