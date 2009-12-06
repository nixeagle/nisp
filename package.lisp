(defpackage #:nisp
  (:use :cl
        #+5am :5am))

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

(in-package :nisp)

(def-suite all-tests
    :description "Top level test suite")