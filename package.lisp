

(defpackage #:nisp-asdf
  (:use :cl #+5am :5am :lift))

(defpackage #:nisp-introspect
  (:use :cl #+5am :5am :lift)
  (:export #:function-lambda-list))


(defpackage #:nispbot-config
  (:use :common-lisp #+5am :5am :lift
        )
  (:export *channel* *nickname* *eighthbit*))