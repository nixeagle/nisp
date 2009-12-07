(defpackage #:nisp
  (:use :cl
        #+5am :5am))

(defpackage #:nisp-introspect
  (:use :cl #+5am :5am)
  (:export #:function-lambda-list))


(defpackage #:nispbot-config
  (:use :common-lisp #+5am :5am
        )
  (:export *channel* *nickname* *eighthbit*))

(defpackage #:nisp-safe
  (:use :common-lisp #+5am :5am))

(defpackage #:nispbot
  (:use :common-lisp  
        :nisp
        #+5am :5am
        :cl-irc :cl-ppcre
       
        :nispbot-config
        :nisp-introspect)
  (:shadowing-import-from :cl-irc :pass))

(in-package :nisp)

(5am:def-suite all-tests
    :description "Top level test suite")

(in-package :nispbot)

(5am:def-suite basic-irc-suite
    :in nisp::all-tests
    :description "Tests all the irc related stuff")

(in-package :nispbot-config)

(5am:def-suite config-suite
    :in nisp::all-tests)

(in-package :nisp-safe)
(5am:def-suite safe-suite
    :in nisp::all-tests)