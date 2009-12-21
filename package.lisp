

(defpackage #:nisp-asdf
  (:use :cl :lift))

(defpackage #:nispbot-config
  (:use :common-lisp :lift
        )
  (:export *channel* *nickname* *eighthbit*))