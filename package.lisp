

(defpackage #:nisp-asdf
  (:use :cl :lift))

(defpackage #:nisp-introspect
  (:use :cl :lift)
  (:export #:function-lambda-list))


(defpackage #:nispbot-config
  (:use :common-lisp :lift
        )
  (:export *channel* *nickname* *eighthbit*))