(defpackage #:nisp-util
  (:use :cl #+5am :5am))

(defpackage #:nisp-random
  (:use :cl #+5am :5am
        :nisp-util))

(defpackage #:nisp-asdf
  (:use :cl #+5am :5am))

(defpackage #:nisp-introspect
  (:use :cl #+5am :5am)
  (:export #:function-lambda-list))


(defpackage #:nispbot-config
  (:use :common-lisp #+5am :5am
        )
  (:export *channel* *nickname* *eighthbit*))

(defpackage #:nisp-safe
  (:use :common-lisp #+5am :5am
        :nisp-util))