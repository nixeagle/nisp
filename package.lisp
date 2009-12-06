(defpackage #:nisp
  (:use :cl
        #+fiveam :5am))

(defpackage #:nisp-introspect
  (:use :cl)
  (:export #:function-lambda-list))


(defpackage #:nispbot-config
  (:use :common-lisp)
  (:export *channel* *nickname* *eighthbit*))

(defpackage #:nispbot
  (:use :common-lisp :irc :cl-ppcre
        :nispbot-config
        :nisp-introspect))
